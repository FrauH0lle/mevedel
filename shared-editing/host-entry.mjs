import { handle } from './host.mjs';
let pending = '';
process.stdin.setEncoding('utf8');
process.stdin.on('data', (chunk) => {
  pending += chunk;
  if (Buffer.byteLength(pending) > 64 * 1024 * 1024) process.exit(2);
  let end;
  while ((end = pending.indexOf('\n')) >= 0) {
    const line = pending.slice(0, end);
    pending = pending.slice(end + 1);
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
