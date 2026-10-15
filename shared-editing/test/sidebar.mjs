/* The discussions sidebar as tests drive it, given the editor FRAME: the
   discussion or new discussion on screen, its box, Ask assistant choice and
   send button, and the sidebar's notice. */
export const shown = frame => frame.locator('.ds-thread:not([hidden])');
export const box = frame => shown(frame).locator('.ds-input');
export const askBox = frame => shown(frame).locator('.ds-choice input');
export const send = frame => shown(frame).locator('.ds-send').click();
export const notice = frame => frame.locator('.ds-notice');
/* Run the entry LABEL of the shown discussion's ⋯ menu. */
export async function menu(frame, label) {
  await shown(frame).locator('.ds-menu > summary').click();
  await shown(frame).locator('.ds-menu-body').getByText(label, {exact: true}).click();
}
/* Show the list of discussions. */
export async function openList(frame) {
  if (!await frame.locator('#assistant').isVisible()) await frame.locator('#ask-toggle').click();
  if (await shown(frame).isVisible()) await shown(frame).locator('.ds-back').click();
}
/* Open the discussion about the whole item. */
export async function openWhole(frame) {
  await openList(frame);
  await frame.locator('.ds-item.general').click();
}
