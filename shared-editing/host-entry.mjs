import { handle } from './host.mjs';
let pending = [], pendingBytes = 0;
process.stdin.setEncoding('utf8');
process.stdin.on('data', (chunk) => {
  let start = 0;
  while (start < chunk.length) {
    const end = chunk.indexOf('\n', start);
    const part = chunk.slice(start, end < 0 ? undefined : end);
    pendingBytes += Buffer.byteLength(part);
    if (pendingBytes > 64 * 1024 * 1024) process.exit(2);
    pending.push(part);
    if (end < 0) break;
    const line = pending.join('');
    pending = []; pendingBytes = 0;
    start = end + 1;
    let request;
    try {
      request = JSON.parse(line);
    } catch {
      process.exit(2);
    }
    Promise.resolve()
      .then(() => handle(request))
      .then(
        (value) =>
          process.stdout.write(JSON.stringify({ requestId: request.requestId, ...value }) + '\n'),
        (error) =>
          process.stdout.write(
            JSON.stringify({
              requestId: request.requestId,
              error: String(error.message).slice(0, 1000),
              ...(error.code === 'stale'
                ? { code: 'stale', targets: error.targets, revision: request.state?.revision }
                : {}),
            }) + '\n',
          ),
      );
  }
});
