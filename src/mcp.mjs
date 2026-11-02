// A Model Context Protocol server over stdio: newline-delimited JSON-RPC
// 2.0 offering the tool registry.  It has no dependencies; it implements
// only what a tools-only server needs.
import { createInterface } from 'node:readline';
import { callTool, tools } from './tools.mjs';

const VERSIONS = ['2025-06-18', '2025-03-26', '2024-11-05'];

/**
 * Serve MCP requests from INPUT to OUTPUT until INPUT ends.  ENV locates
 * the link store.
 */
export async function serve({ input, output, env = process.env }) {
  const ctx = { env, client: undefined };
  const write = (message) => output.write(`${JSON.stringify({ jsonrpc: '2.0', ...message })}\n`);
  const pending = new Set();
  const handle = async (message) => {
    const { id, method, params } = message;
    const reply = (result) => id !== undefined && write({ id, result });
    const fail = (code, text) => id !== undefined && write({ id, error: { code, message: text } });
    switch (method) {
      case 'initialize':
        ctx.client = params?.clientInfo?.name;
        return reply({
          protocolVersion: VERSIONS.includes(params?.protocolVersion) ? params.protocolVersion : VERSIONS[0],
          capabilities: { tools: {} },
          serverInfo: { name: 'mevedel-remote-artifact', version: '0.1.0' },
        });
      case 'ping':
        return reply({});
      case 'tools/list':
        return reply({ tools: tools.map(({ name, description, inputSchema }) => ({ name, description, inputSchema })) });
      case 'tools/call': {
        const result = await callTool(params?.name, params?.arguments, ctx);
        return result ? reply(result) : fail(-32602, `Unknown tool: ${params?.name}`);
      }
      default:
        if (method?.startsWith('notifications/')) return undefined;
        return fail(-32601, `Method not found: ${method}`);
    }
  };
  const lines = createInterface({ input, crlfDelay: Infinity });
  for await (const line of lines) {
    if (!line.trim()) continue;
    let message;
    try {
      message = JSON.parse(line);
    } catch {
      write({ id: null, error: { code: -32700, message: 'Parse error' } });
      continue;
    }
    const work = handle(message).catch((error) => message.id !== undefined
      && write({ id: message.id, error: { code: -32603, message: error.message } }));
    pending.add(work);
    work.finally(() => pending.delete(work));
  }
  await Promise.all(pending);
}
