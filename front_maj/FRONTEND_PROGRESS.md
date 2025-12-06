# Frontend Progress (Riichi Mahjong Prototype)

## What’s working
- Four-player layout on green table with enlarged south hand, opponent backs, dora panel, wall counter, discard piles per seat.
- Player flow: draw ➜ select tile ➜ discard; auto-sort by suit/number; win/Rong modal showing full 13 tiles (hand + draw slot + melds) with enlarged visuals.
- Actions bar (Chi/Peng/Rong/Skip) appears on mocked triggers; Chi/Peng move opponent discard + two of your tiles into meld area; Rong shares the Win modal.
- Meld area pinned to south hand; draw slot holds the just-drawn tile until discarded/merged.
- Discard piles oriented per seat (rotation) with consistent spacing.

## Mocked assumptions (no backend yet)
- Tile scores: random 0–100 via `fetchTileScore` mock.
- Win check: always `true` via `checkWinCondition`.
- Action availability: `fetchActionOptions` returns Chi/Peng/Rong enabled only on every 5th AI discard turn (toggle stub).
- Meld composition: Chi/Peng randomly pulls two tiles from player hand plus last opponent discard; real logic to be driven by backend.
- Win summary text: static placeholder from `fetchWinSummary`.

## Backend hook points
- `api.js`
  - `fetchTileScore(handTiles)` → replace with real scoring API (expects map: tileId → score).
  - `checkWinCondition(handTiles)` → real win/ron check.
  - `fetchActionOptions(turnIndex)` → real Chi/Peng/Rong availability per discard context.
  - `fetchWinSummary()` → real win description/points.
- Rendering/flow touchpoints in `main.js`
  - Scores refreshed after draw/discard/meld via `refreshScores`.
  - Action buttons shown in `promptAction` based on `fetchActionOptions` result.
  - Meld handling in `performMeld` (replace random picks with backend-dictated tiles).
  - Win modal fed by `openWinModal(getFullTilesForWin())`.
  - Sorting via `sortTiles` (utils.js) already used whenever south hand renders.

## Run locally
```bash
npx http-server . -p 8080
# then open
http://127.0.0.1:8080
```
