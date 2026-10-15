import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {editorFixture} from './editor-fixture.mjs';

// The packaged room page with a stub transport: the header, its ⋯ menu and
// the Invite sheet act in the page alone.
async function room(t, name) {
  const {browser,url}=await editorFixture(t),context=await browser.newContext();
  await context.route('**/transport.js',async route => {
    const source=await readFile(new URL('../../relay/viewer/transport.js',import.meta.url),'utf8');
    await route.fulfill({contentType:'text/javascript',body:source+`
      window.mevedelViewerTransport={...window.mevedelViewerTransport,create(options){
        window.sentTest=[];
        return {connect(){
          options.onConnection('Connected','connected');
          options.onFrame({t:'welcome',readOnly:false,commands:[]});
          options.onFrame({t:'snapshot-chunk',final:true,records:[]});
          options.onFrame({t:'status',busy:false,name:${JSON.stringify(name)}});
        },end(){},async send(frame){ window.sentTest.push(frame); return true; }};
      }};`});
  });
  const page=await context.newPage();page.setDefaultTimeout(5000);
  const errors=[];page.on('pageerror',error=>errors.push(error.message));
  await page.goto(`${url}/room#designroom000001.${Buffer.alloc(64).toString('base64url')}`);
  await page.locator('#session-label').getByText(name,{exact:true}).waitFor();
  return {page,errors};
}

// The header refits on the frame after a change.
const settle = page => page.evaluate(() => new Promise(done => requestAnimationFrame(() => requestAnimationFrame(done))));
const head = page => page.locator('body > .topbar');

test('the header keeps one line and folds actions into its menu', async t => {
  const {page,errors}=await room(t,'SNT Prevalence/Incidence Architecture Planning');
  await page.setViewportSize({width:1280,height:800});await settle(page);
  assert.equal(await head(page).locator('.head-more').isVisible(),false,'nothing folds on a wide screen');
  for (const control of ['#appearance-menu > summary','#invite-button','#notify-button'])
    assert.equal(await head(page).locator(control).isVisible(),true,control);

  await page.setViewportSize({width:390,height:844});await settle(page);
  const bar=await head(page).boundingBox();
  assert.equal(bar.height,56,'one header line on a phone');
  const tops=await head(page).locator('.wordmark, .room-heading, .header-actions > :not([hidden])')
    .evaluateAll(nodes=>nodes.map(node=>node.getBoundingClientRect()).map(b=>[b.top,b.bottom]));
  for (const [top,bottom] of tops) assert.ok(top>=bar.y&&bottom<=bar.y+bar.height,JSON.stringify(tops));
  const title=await page.locator('#session-label').boundingBox();
  assert.ok(title.width>=150,`the title keeps its share: ${title.width}`);
  assert.equal(await head(page).locator('.head-more > summary').isVisible(),true);
  assert.equal(await page.locator('#invite-button').isVisible(),false,'Invite folds before the title shrinks');
  assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));

  // A folded action works from the menu, which closes behind it.
  await head(page).locator('.head-more > summary').click();
  await page.locator('#invite-button').click();
  assert.equal(await page.locator('#invite').evaluate(dialog=>dialog.open),true);
  assert.equal(await head(page).locator('.head-more').getAttribute('open'),null);
  await page.locator('#invite').getByRole('button',{name:'Done',exact:true}).click();

  await page.setViewportSize({width:1280,height:800});await settle(page);
  assert.equal(await head(page).locator('.head-more').isVisible(),false);
  const order=await head(page).locator('.header-actions > *:not(.head-more)').evaluateAll(nodes=>nodes.map(n=>n.id||n.className));
  assert.deepEqual(order.filter(Boolean),['you-menu','presence keep','appearance-menu','invite-button','notify-button'],'actions return in order');
  assert.deepEqual(errors,[]);
});

test('Invite shows a tier as a QR code only when asked', async t => {
  const {page,errors}=await room(t,'Planning');
  await page.locator('#invite-button').click();
  const sheet=page.locator('#invite');
  assert.equal(await sheet.locator('#invite-qr').isVisible(),false,'no code shows by itself');
  const buttons=sheet.getByRole('button',{name:'QR code',exact:true});
  assert.equal(await buttons.count(),3,'an owner link offers every tier');
  await buttons.nth(0).click();
  assert.equal(await sheet.locator('#invite-qr svg').isVisible(),true);
  assert.match(await sheet.locator('#invite-qr figcaption').innerText(),/view access/);
  const view=await sheet.locator('#invite-qr svg path').getAttribute('d');
  await buttons.nth(2).click();
  assert.equal(await buttons.nth(0).getAttribute('aria-pressed'),'false');
  assert.equal(await buttons.nth(2).getAttribute('aria-pressed'),'true');
  assert.match(await sheet.locator('#invite-qr figcaption').innerText(),/owner access/);
  assert.notEqual(await sheet.locator('#invite-qr svg path').getAttribute('d'),view,'each tier has its own code');
  // The code stays dark on white in a dark theme, for any camera.
  assert.equal(await sheet.locator('#invite-qr svg rect').getAttribute('fill'),'#fff');
  await buttons.nth(2).click();
  assert.equal(await sheet.locator('#invite-qr').isVisible(),false,'pressing again hides the code');
  assert.deepEqual(errors,[]);
});
