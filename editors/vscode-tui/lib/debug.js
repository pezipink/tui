'use strict';

// Pure helpers for the debug configuration: where the built exe goes, the build command, and the
// launch configuration handed to the C# extension's clr debugger.

const crypto = require('crypto');
const os = require('os');
const path = require('path');

// The exe for a source file. The hash of the full path keeps two files with one name apart.
function exePathFor(source, dir = path.join(os.tmpdir(), 'tui-debug')) {
  const hash = crypto.createHash('sha1').update(source).digest('hex').slice(0, 8);
  return path.join(dir, `${path.basename(source, path.extname(source))}-${hash}.exe`);
}

// The arguments for racket: build.rkt takes the source and the output exe.
function buildArgs(buildScript, source, exe) {
  return [buildScript, source, exe];
}

// The file a launch configuration names: its source attribute, or the active file.
function sourceOf(request, activeFile) {
  return request.source || activeFile;
}

// A launch configuration for the clr debugger. The PDB written beside the exe carries the
// line table, so breakpoints in the .tui file resolve against it.
function clrConfig(exe, request) {
  return {
    name: request.name || 'Tui program',
    type: 'clr',
    request: 'launch',
    program: exe,
    cwd: path.dirname(exe),
    args: request.args || [],
    console: request.console || 'internalConsole',
    stopAtEntry: Boolean(request.stopAtEntry),
  };
}

module.exports = { exePathFor, buildArgs, sourceOf, clrConfig };
