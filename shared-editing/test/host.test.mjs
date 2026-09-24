import test from 'node:test';
import assert from 'node:assert/strict';
import { handle } from '../host.mjs';

test('large retained snapshots expire before repeated moves block new saves', async () => {
  const content = Array.from({length:50},(_,i)=>({id:`shape-${i}`,type:'rect',box:[i*10,0,100,100],text:'x'.repeat(10000)}));
  let {state}=await handle({action:'create',id:'large-history',kind:'whiteboard',title:'Large history',content,actor:'Guest: Alice',opId:'create'});
  for(let step=0;step<20;step++) {
    const {result}=await handle({action:'read',state});
    const changes=result.content.map(shape=>({id:shape.id,before:shape,after:{...shape,box:[shape.box[0],step+1,100,100]}}));
    ({state}=await handle({action:'patch',state,actor:'Guest: Alice',opId:`move-${step}`,changes}));
  }
  assert.ok(state.transactions.length<20,'old snapshots expire by size as well as count');
  assert.ok(Buffer.byteLength(JSON.stringify(state.transactions))<=4*1024*1024);
  const latest=state.transactions[0];
  assert.equal(latest.id,'move-19');
  const read=await handle({action:'read',state,since:0});
  assert.equal(read.result.historyTruncated,true);
  const reverted=await handle({action:'revert',state,actor:'Agent: test',opId:'revert',transaction:latest.id});
  assert.equal(reverted.result.content[0].box[1],19,'retained entries still revert');
  const duplicate=await handle({action:'patch',state,actor:'Guest: Alice',opId:'move-0',changes:[]});
  assert.equal(duplicate.result.revision,state.revision,'expired history does not remove replay receipts');
});

test('availability verifies schema and renderer without returning durable state', async () => {
  assert.deepEqual(await handle({ action: 'status' }), { result: { available: true } });
});

test('availability rejects unsupported Node versions with an actionable reason', async () => {
  const descriptor = Object.getOwnPropertyDescriptor(process.versions, 'node');
  try {
    for (const version of ['20.19.0', '22.3.0']) {
      Object.defineProperty(process.versions, 'node', { value: version });
      await assert.rejects(handle({ action: 'status' }), /Node 22.4 or newer on the Emacs host/);
    }
  } finally {
    Object.defineProperty(process.versions, 'node', descriptor);
  }
});

test('host creates, exports, imports independently, and renders without a browser', async () => {
  const made = await handle({
    action: 'create',
    id: 'board1',
    kind: 'whiteboard',
    title: 'Design',
    actor: 'Alice',
    opId: 'create1',
  });
  const edited = await handle({
    action: 'patch',
    state: made.state,
    actor: 'Agent',
    opId: 'edit1',
    image: true,
    changes: [
      {
        id: 'one',
        before: null,
        after: { id: 'one', type: 'rect', box: [0, 0, 100, 50], text: 'Client' },
      },
    ],
  });
  const retried = await handle({
    action: 'patch',
    state: edited.state,
    actor: 'Agent',
    opId: 'edit1',
    image: true,
    changes: [],
  });
  assert.equal(retried.state.revision, edited.state.revision);
  const png = await handle({ action: 'export', state: edited.state, format: 'png' });
  assert.equal(
    Buffer.from(png.result.data, 'base64').subarray(0, 8).toString('hex'),
    '89504e470d0a1a0a',
  );
  assert.equal(edited.result.png, png.result.data, 'edit returns the committed board image');
  assert.equal(retried.result.png, png.result.data, 'retry returns the same image');
  const native = await handle({ action: 'export', state: edited.state, format: 'native' });
  const copy = await handle({
    action: 'import',
    format: 'native',
    id: 'board2',
    data: native.result.text,
    actor: 'Alice',
    opId: 'import1',
  });
  assert.notEqual(copy.state.id, edited.state.id);
  assert.deepEqual(copy.result.content, edited.result.content);
  assert.equal(copy.state.revision, 1);
  assert.equal(edited.state.transactions[0].actor, 'Agent');
  assert.ok(Number.isSafeInteger(edited.state.transactions[0].time));
  assert.equal(retried.state.transactions[0].time, edited.state.transactions[0].time);
});

test('selected connector PNG uses the current bound endpoint and includes its reference', async () => {
  const arrow = {
    id: 'arrow',
    type: 'arrow',
    box: [0, 0, 100, 100],
    points: [
      [0, 0],
      [100, 100],
    ],
    to: 'target',
  };
  const target = { id: 'target', type: 'rect', edges: 'sharp', box: [500, 500, 100, 100] };
  const board = await handle({
    action: 'create',
    id: 'bound',
    kind: 'whiteboard',
    title: 'Bound',
    actor: 'Alice',
    opId: 'one',
    content: [arrow, target],
  });
  const selected = await handle({
    action: 'read',
    state: board.state,
    selection: ['arrow'],
    image: true,
    imageMax: 512,
  });
  assert.deepEqual(selected.result.content, [arrow]);
  assert.deepEqual(selected.result.context, [target]);
  const resolved = {
    ...arrow,
    box: [0, 0, 500, 500],
    points: [
      [0, 0],
      [500, 500],
    ],
  };
  delete resolved.to;
  const expected = await handle({
    action: 'create',
    id: 'resolved',
    kind: 'whiteboard',
    title: 'Resolved',
    actor: 'Alice',
    opId: 'two',
    content: [resolved],
  });
  const image = await handle({ action: 'read', state: expected.state, image: true, imageMax: 512 });
  assert.equal(selected.result.png, image.result.png);
});

test('targeted inverses preserve independent work and refuse overlapping changes', async () => {
  const p = (id, text) => ({ type: 'paragraph', attrs: { id }, content: [{ type: 'text', text }] });
  const base = await handle({
    action: 'create',
    id: 'doc',
    kind: 'document',
    actor: 'Guest',
    opId: 'create',
    content: { type: 'doc', content: [p('a', 'A'), p('b', 'B'), p('c', 'C')] },
  });
  const edit = await handle({
    action: 'patch',
    state: base.state,
    actor: 'Agent',
    opId: 'delete',
    changes: base.result.content.content
      .slice(0, 2)
      .map((n) => ({ id: n.attrs.id, before: n, after: null })),
  });
  const human = await handle({
    action: 'patch',
    state: edit.state,
    actor: 'Guest',
    opId: 'human',
    changes: [{ id: 'c', before: edit.result.content.content[0], after: p('c', 'Human') }],
  });
  const reverted = await handle({
    action: 'revert',
    state: human.state,
    actor: 'Guest',
    opId: 'revert',
    transaction: 'delete',
  });
  assert.deepEqual(
    reverted.result.content.content.map((n) => n.content[0].text),
    ['A', 'B', 'Human'],
  );
  const renamed = await handle({
    action: 'rename',
    state: reverted.state,
    actor: 'Agent',
    opId: 'rename',
    title: 'Renamed',
  });
  const later = await handle({
    action: 'rename',
    state: renamed.state,
    actor: 'Guest',
    opId: 'later',
    title: 'Human title',
  });
  await assert.rejects(
    handle({
      action: 'revert',
      state: later.state,
      actor: 'Guest',
      opId: 'bad',
      transaction: 'rename',
    }),
    /Stale title/,
  );
  const overlap = await handle({
    action: 'patch',
    state: human.state,
    actor: 'Guest',
    opId: 'overlap',
    changes: [{ id: 'c', before: human.result.content.content[0], after: p('c', 'Later human') }],
  });
  await assert.rejects(
    handle({
      action: 'revert',
      state: overlap.state,
      actor: 'Guest',
      opId: 'bad2',
      transaction: 'human',
    }),
    (e) => e.code === 'stale',
  );
});

test('native assets and viewable exports round trip; malformed imports stay unpublished', async () => {
  const board = await handle({
    action: 'create',
    id: 'board',
    kind: 'whiteboard',
    actor: 'Guest',
    opId: 'create',
  });
  const png = (await handle({ action: 'export', state: board.state, format: 'png' })).result.data;
  const shape = {
    id: 'image',
    type: 'image',
    box: [0, 0, 100, 100],
    src: `data:image/png;base64,${png}`,
  };
  const source = {
    format: 'mevedel-editable-1',
    kind: 'whiteboard',
    title: 'Image',
    content: [shape],
  };
  const imported = await handle({
    action: 'import',
    format: 'native',
    id: 'imageboard',
    actor: 'Guest',
    opId: 'import',
    data: JSON.stringify(source),
  });
  const exported = await handle({ action: 'export', state: imported.state, format: 'native' });
  assert.deepEqual(JSON.parse(exported.result.text), source);
  assert.match(
    (await handle({ action: 'export', state: imported.state, format: 'svg' })).result.text,
    /data:image\/png;base64/,
  );
  const doc = await handle({
    action: 'import',
    format: 'markdown',
    id: 'markdown',
    actor: 'Guest',
    opId: 'md',
    data: '# Notes\n\n**Bold** and [link](https://example.com)\n',
  });
  assert.match(
    (await handle({ action: 'export', state: doc.state, format: 'markdown' })).result.text,
    /# Notes/,
  );
  assert.match(
    (await handle({ action: 'export', state: doc.state, format: 'html' })).result.text,
    /<strong>Bold<\/strong>/,
  );
  for (const invalid of [
    { ...source, script: 'alert(1)' },
    { ...source, content: [{ ...shape, src: 'https://example.com/secret.png' }] },
    { ...source, content: [{ ...shape, src: shape.src.slice(0, -20) }] },
    { ...source, content: [{ ...shape, src: undefined }] },
    { ...source, content: [shape, shape] },
  ])
    await assert.rejects(
      handle({
        action: 'import',
        format: 'native',
        id: 'invalid',
        actor: 'Guest',
        opId: 'bad',
        data: JSON.stringify(invalid),
      }),
    );
  await assert.rejects(
    handle({
      action: 'import',
      format: 'html',
      id: 'invalid',
      actor: 'Guest',
      opId: 'bad',
      data: '<script>bad</script>',
    }),
    /Unsupported import/,
  );
  assert.equal(board.state.revision, 1);
});

test('document images survive saves and native, HTML, and Markdown exports', async () => {
  const board = await handle({action:'create',id:'image-source',kind:'whiteboard',actor:'Guest',opId:'source'});
  const src = 'data:image/png;base64,' + (await handle({action:'export',state:board.state,format:'png'})).result.data;
  const image = {type:'image',attrs:{id:'picture',src,alt:'Diagram',width:320,height:160}};
  const made = await handle({action:'create',id:'illustrated',kind:'document',actor:'Guest',opId:'create',
    content:{type:'doc',content:[image]}});
  const read = await handle({action:'read',state:made.state});
  assert.equal(read.result.content.content[0].attrs.src,src);
  for (const format of ['native','markdown']) {
    const exported = await handle({action:'export',state:made.state,format});
    const imported = await handle({action:'import',id:'copy-'+format,format,data:exported.result.text,actor:'Guest',opId:'copy'});
    assert.equal(imported.result.content.content.find(n=>n.type==='image').attrs.src,src);
  }
  const html = await handle({action:'export',state:made.state,format:'html'});
  assert.ok(html.result.text.includes(`<img src="${src}" alt="Diagram" width="320" height="160">`));
  for (const attrs of [{src:'https://example.com/image.png'}, {src:'data:image/svg+xml;base64,PHN2Zz4='},
                        {src:'data:image/png;base64,YmFk'}, {width:-1}, {alt:{bad:true}}]) {
    await assert.rejects(handle({action:'patch',state:made.state,actor:'Guest',opId:'bad',changes:[
      {id:'picture',before:read.result.content.content[0],after:{type:'image',attrs:{...image.attrs,...attrs}}},
    ]}));
  }
  assert.equal((await handle({action:'read',state:made.state})).result.content.content[0].attrs.src,src);
});

test('image edits validate at the host and retain their original through native import', async () => {
  const {restore,encode,inspect,patch,applyUpdate} = await import('../model.mjs');
  const source = await handle({action:'create',id:'source',kind:'whiteboard',actor:'Guest',opId:'source'});
  const src = 'data:image/png;base64,'+(await handle({action:'export',state:source.state,format:'png'})).result.data;
  const imageEdit = {src,crop:[0.2,0.1,0.5,0.6],rotation:90,flipX:true,flipY:false};
  for (const kind of ['whiteboard','document']) {
    const image = kind === 'whiteboard'
      ? {id:'picture',type:'image',box:[0,0,100,100],src,imageEdit}
      : {type:'image',attrs:{id:'picture',src,imageEdit,width:100,height:100}};
    const content = kind==='whiteboard' ? [image] : {type:'doc',content:[image]};
    const {state} = await handle({action:'create',id:'edited-'+kind,kind,content,actor:'Guest',opId:'create'});
    const native = await handle({action:'export',format:'native',state});
    const imported = await handle({action:'import',format:'native',id:'copy-'+kind,data:native.result.text,actor:'Guest',opId:'copy'});
    const attrs = kind==='whiteboard' ? imported.result.content[0] : imported.result.content.content[0].attrs;
    assert.equal(attrs.src,src);assert.deepEqual(attrs.imageEdit,imageEdit);
    for (const invalid of [
      {...imageEdit,crop:[0,0,0,1]}, {...imageEdit,crop:[0.8,0,0.5,1]},
      {...imageEdit,crop:[0,0,1]}, {...imageEdit,crop:[0,0,'1',1]},
      {...imageEdit,rotation:45}, {...imageEdit,flipX:'true'},
      {...imageEdit,src:'https://example.com/image.png'}, {...imageEdit,unknown:true},
    ]) {
      const after = kind==='whiteboard' ? {...image,imageEdit:invalid} : {...image,attrs:{...image.attrs,imageEdit:invalid}};
      const read = await handle({action:'read',state});
      const before = kind==='whiteboard' ? read.result.content[0] : read.result.content.content[0];
      await assert.rejects(handle({action:'patch',state,actor:'Guest',opId:'bad',changes:[{id:'picture',before,after}]}),/image (edit|crop|orientation)|embedded image/i);
    }
    const peers = [restore(Buffer.from(state.crdt,'base64')),restore(Buffer.from(state.crdt,'base64'))];
    try {
      const edits = [{...imageEdit,rotation:180}, {...imageEdit,flipY:true}];
      peers.forEach((peer,i) => {
        const content=inspect(peer).content;
        const before=kind==='whiteboard'?content[0]:content.content[0];
        if (kind==='whiteboard') patch(peer,[{id:'picture',before,after:{...before,imageEdit:edits[i]}}]);
        // Tiptap's attribute update edits the surviving image node, unlike a
        // host patch which deliberately replaces a target-checked whole block.
        else peer.getXmlFragment('document').get(0).setAttribute('imageEdit',edits[i]);
      });
      applyUpdate(peers[0],encode(peers[1]));applyUpdate(peers[1],encode(peers[0]));
      assert.deepEqual(inspect(peers[0]),inspect(peers[1]),'concurrent image edits converge');
      const content=inspect(peers[0]).content;
      const merged=kind==='whiteboard'?content[0]:content.content[0].attrs;
      assert.equal(merged.src,src);
      assert.ok(edits.some(edit=>JSON.stringify(edit)===JSON.stringify(merged.imageEdit)),
        'settings and rendered pixels remain one complete edit');
    } finally {peers.forEach(peer=>peer.destroy());}
  }
});
