/**
 * Utility functions for tile handling and ordering.
 */

/**
 * Derive comparable data from a tile code.
 * @param {string} code - e.g. "1W", "9T", "5S", "RD"
 * @returns {{suit: string, rank: number}}
 */
export function parseTile(code) {
  const suitOrder = ["W", "T", "S", "Z"];

  const honorRanks = ["E", "S", "W", "N", "RD", "GD", "WD"];
  const match = code.match(/^(\d+)([WTS])$/);

  if (match) {
    const [, num, suit] = match;
    return { suit, rank: parseInt(num, 10) };
  }

  const honorIndex = honorRanks.indexOf(code);
  return { suit: "Z", rank: honorIndex >= 0 ? honorIndex + 1 : 99 };
}

/**
 * Sort tiles by suit then by rank.
 * Order: Characters(W) -> Dots(T) -> Bamboos(S) -> Honors(Z)
 * @param {Array<{code: string}>} tiles
 * @returns {Array<{code: string}>}
 */
export function sortTiles(tiles) {
  const suitPriority = { W: 0, T: 1, S: 2, Z: 3 };

  return [...tiles].sort((a, b) => {
    const tileA = parseTile(a.code);
    const tileB = parseTile(b.code);

    if (tileA.suit !== tileB.suit) {
      return suitPriority[tileA.suit] - suitPriority[tileB.suit];
    }
    return tileA.rank - tileB.rank;
  });
}
