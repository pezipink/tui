'use strict';

// The state a request needs from a document: its file, version and text.
function documentState(document) {
  return { file: document.uri.fsPath, version: document.version, text: document.getText() };
}

// Answers the completion and browse requests for documents. The helper expands a document only
// when its version changes, and requests for the same version share that expansion.
class TuiService {
  constructor(client) {
    this.client = client;
    this.synced = new Map(); // file -> { version, done }
    this.typeCache = new Map(); // file -> { version, types }
  }

  sync(doc) {
    const cached = this.synced.get(doc.file);
    if (cached && cached.version === doc.version) return cached.done;
    const done = this.client.scopes(doc.file, doc.text).catch((err) => {
      // A failed expansion keeps the helper's last good scopes, so the other requests still answer.
      if (err.reply && err.reply.kept) return err.reply;
      throw err;
    });
    this.synced.set(doc.file, { version: doc.version, done });
    return done;
  }

  async names(doc, offset) {
    await this.sync(doc);
    return (await this.client.names(doc.file, offset)).names;
  }

  async types(doc) {
    const cached = this.typeCache.get(doc.file);
    if (cached && cached.version === doc.version) return cached.types;
    await this.sync(doc);
    const types = (await this.client.types(doc.file)).types;
    this.typeCache.set(doc.file, { version: doc.version, types });
    return types;
  }

  async members(doc, offset, receiver) {
    await this.sync(doc);
    return (await this.client.members(doc.file, offset, receiver)).members;
  }

  // Drops the cached state for a document that has been closed.
  forget(file) {
    this.synced.delete(file);
    this.typeCache.delete(file);
  }
}

module.exports = { TuiService, documentState };
