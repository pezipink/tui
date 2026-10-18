'use strict';

const { spawn } = require('child_process');

// A reply with ok false. The reply is kept, so callers can see fields such as kept.
class HelperError extends Error {
  constructor(message, reply) {
    super(message);
    this.reply = reply;
  }
}

// Talks to private/meta-server.rkt over stdio. One helper process serves every request. Requests
// are answered in order, and each reply carries the id of its request.
class MetaClient {
  // opts: { racket, server, cwd, timeoutMs, log }
  constructor(opts) {
    this.opts = Object.assign({ timeoutMs: 20000, log: () => {} }, opts);
    this.proc = null;
    this.buffer = '';
    this.nextId = 1;
    this.pending = new Map();
  }

  start() {
    if (this.proc) return;
    const proc = spawn(this.opts.racket, [this.opts.server], {
      cwd: this.opts.cwd,
      windowsHide: true,
      stdio: ['pipe', 'pipe', 'pipe'],
    });
    this.proc = proc;
    this.buffer = '';
    proc.stdout.setEncoding('utf8');
    proc.stdout.on('data', (chunk) => this.onData(chunk));
    proc.stderr.setEncoding('utf8');
    proc.stderr.on('data', (text) => this.opts.log(text));
    proc.stdin.on('error', (err) => this.die(proc, err));
    proc.on('error', (err) => this.die(proc, err));
    proc.on('exit', (code) =>
      this.die(proc, new Error(`the metadata helper exited with code ${code}`)));
  }

  // Forgets a helper that has gone away and fails the requests it still owed.
  die(proc, err) {
    if (this.proc !== proc) return;
    this.proc = null;
    const owed = [...this.pending.values()];
    this.pending.clear();
    for (const p of owed) p.reject(err);
  }

  request(op, fields) {
    this.start();
    const id = this.nextId++;
    const line = JSON.stringify(Object.assign({ op, id }, fields)) + '\n';
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        this.pending.delete(id);
        reject(new Error(`the metadata helper did not answer ${op} in ${this.opts.timeoutMs} ms`));
      }, this.opts.timeoutMs);
      this.pending.set(id, {
        resolve: (reply) => { clearTimeout(timer); resolve(reply); },
        reject: (err) => { clearTimeout(timer); reject(err); },
      });
      this.proc.stdin.write(line);
    });
  }

  onData(chunk) {
    this.buffer += chunk;
    let nl;
    while ((nl = this.buffer.indexOf('\n')) >= 0) {
      const line = this.buffer.slice(0, nl).trim();
      this.buffer = this.buffer.slice(nl + 1);
      if (line) this.onLine(line);
    }
  }

  onLine(line) {
    let reply;
    try {
      reply = JSON.parse(line);
    } catch (err) {
      this.opts.log(`unreadable reply from the metadata helper: ${line}\n`);
      return;
    }
    const p = this.pending.get(reply.id);
    if (!p) return; // the request timed out, or the reply is for an earlier helper
    this.pending.delete(reply.id);
    if (reply.ok) p.resolve(reply);
    else p.reject(new HelperError(reply.error || 'the metadata helper failed', reply));
  }

  // The protocol requests. Each resolves to the reply object.
  scopes(file, text) {
    return this.request('scopes', { file, text });
  }

  names(file, offset) {
    return this.request('names', { file, offset });
  }

  types(file) {
    return this.request('types', { file });
  }

  members(file, offset, receiver) {
    return this.request('members', { file, offset, receiver });
  }

  dispose() {
    const proc = this.proc;
    if (!proc) return;
    this.die(proc, new Error('the metadata helper was stopped'));
    proc.stdin.end();
    proc.kill();
  }
}

module.exports = { MetaClient, HelperError };
