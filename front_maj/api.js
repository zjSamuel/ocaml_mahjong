/**
 * Mock API layer.
 * These functions mimic async calls so real endpoints can replace them later.
 */

/**
 * Request recommended score for each tile.
 * @param {Array<{id: string, code: string}>} handTiles
 * @returns {Promise<Record<string, number>>} map of tile id -> score
 */
export async function fetchTileScore(handTiles) {
  // Simulate latency
  await delay(120);
  const scores = {};
  handTiles.forEach(tile => {
    const randomScore = parseFloat((Math.random() * 100).toFixed(1));
    scores[tile.id] = randomScore;
  });
  return scores;
}

/**
 * Validate winning hand.
 * @param {Array<{code: string}>} handTiles
 * @returns {Promise<boolean>}
 */
export async function checkWinCondition(handTiles) {
  await delay(100);
  // Always true for prototype; replace with real validation later.
  return true;
}

/**
 * Fetch additional win summary text.
 * @returns {Promise<string>}
 */
export async function fetchWinSummary() {
  await delay(80);
  // Replace with backend-provided summary.
  return "Win type: XYZ, Score: +8000";
}

/**
 * Fetch available actions after an opponent discard.
 * @param {number} turnIndex 1-based turn counter from AI discards
 * @returns {Promise<{canChi: boolean, canPeng: boolean, canRong: boolean}>}
 */
export async function fetchActionOptions(turnIndex) {
  await delay(120);
  // Mock: only every 5th AI discard offers reactions.
  const enabled = turnIndex % 5 === 0;
  return {
    canChi: enabled,
    canPeng: enabled,
    canRong: enabled,
  };
}

function delay(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}
