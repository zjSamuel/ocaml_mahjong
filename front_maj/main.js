import { sortTiles } from "./utils.js";
import { fetchTileScore, checkWinCondition, fetchActionOptions, fetchWinSummary } from "./api.js";

// --- DOM references ---
const southHandEl = document.getElementById("south-hand");
const northHandEl = document.getElementById("north-hand");
const eastHandEl = document.getElementById("east-hand");
const westHandEl = document.getElementById("west-hand");
const discardEls = {
  north: document.getElementById("discard-north"),
  south: document.getElementById("discard-south"),
  east: document.getElementById("discard-east"),
  west: document.getElementById("discard-west"),
};
const remainingEl = document.getElementById("remaining-count");
const drawBtn = document.getElementById("draw-btn");
const discardBtn = document.getElementById("discard-btn");
const winBtn = document.getElementById("win-btn");
const actionBtns = {
  chi: document.getElementById("chi-btn"),
  peng: document.getElementById("peng-btn"),
  rong: document.getElementById("rong-btn"),
  skip: document.getElementById("skip-btn"),
};
const doraTilesEl = document.getElementById("dora-tiles");
const modalOverlay = document.getElementById("win-modal");
const modalHandEl = document.getElementById("modal-hand");
const modalSummaryEl = document.getElementById("win-summary");
const closeModalBtn = document.getElementById("close-modal");
const continueBtn = document.getElementById("continue-btn");
const meldsEl = document.getElementById("melds");
const drawSlotEl = document.getElementById("draw-slot");

// --- Game state ---
let deck = [];
let playerHand = [];
let aiHands = {
  north: new Array(13).fill(null),
  east: new Array(13).fill(null),
  west: new Array(13).fill(null),
};
let discards = {
  north: [],
  south: [],
  east: [],
  west: [],
};
let doraIndicators = [];
let remainingTiles = 0;
let tileScores = {};
let canDiscard = false;
let idCounter = 0;
let selectedTileId = null;
let selectedTileSource = null; // "hand" | "draw"
let drawnTile = null;
let melds = [];
let aiTurnCounter = 0;
let pendingActionResolve = null;
let pendingActionContext = null;
let gameOver = false;

// --- Initialization ---
initGame();

function initGame() {
  discards = { north: [], south: [], east: [], west: [] };
  aiHands = {
    north: [],
    east: [],
    west: [],
  };
  tileScores = {};
  selectedTileId = null;
  selectedTileSource = null;
  drawnTile = null;
  melds = [];
  aiTurnCounter = 0;
  pendingActionResolve = null;
  pendingActionContext = null;
  gameOver = false;
  canDiscard = false;

  deck = buildDeck();
  shuffle(deck);
  doraIndicators = deck.splice(0, 5);
  // Deal 13 tiles to each player
  playerHand = drawMany(13);
  aiHands.north = drawMany(13);
  aiHands.east = drawMany(13);
  aiHands.west = drawMany(13);
  remainingTiles = deck.length;
  refreshScores();
  renderAll();
}

// --- Core helpers ---
function buildDeck() {
  const tiles = [];
  const suits = ["W", "T", "S"];
  suits.forEach(suit => {
    for (let num = 1; num <= 9; num++) {
      for (let i = 0; i < 4; i++) {
        tiles.push(createTile(`${num}${suit}`));
      }
    }
  });
  const honors = ["E", "S", "W", "N", "RD", "GD", "WD"];
  honors.forEach(code => {
    for (let i = 0; i < 4; i++) {
      tiles.push(createTile(code));
    }
  });
  return tiles;
}

function createTile(code) {
  return { id: `t-${idCounter++}`, code };
}

function shuffle(arr) {
  for (let i = arr.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [arr[i], arr[j]] = [arr[j], arr[i]];
  }
}

function drawMany(count) {
  const drawn = [];
  for (let i = 0; i < count; i++) {
    const tile = deck.pop();
    if (tile) drawn.push(tile);
  }
  remainingTiles = deck.length;
  return drawn;
}

async function refreshScores() {
  const tilesForScore = drawnTile ? [...playerHand, drawnTile] : playerHand;
  tileScores = await fetchTileScore(tilesForScore);
  renderSouthHand();
}

// --- Rendering ---
function renderAll() {
  renderSouthHand();
  renderOpponents();
  renderDiscards();
  renderRemaining();
  renderDora();
  renderMelds();
  renderDrawSlot();
  updateButtons();
}

function renderSouthHand() {
  const sorted = sortTiles(playerHand);
  playerHand = sorted;
  southHandEl.innerHTML = "";

  sorted.forEach(tile => {
    const tileEl = document.createElement("div");
    tileEl.className = "tile";
    tileEl.textContent = tile.code;
    const scoreEl = document.createElement("div");
    scoreEl.className = "score";
    scoreEl.textContent = tileScores[tile.id] !== undefined ? tileScores[tile.id] : "-";
    tileEl.appendChild(scoreEl);
    if (canDiscard) {
      tileEl.classList.add("playable");
      if (selectedTileId === tile.id) {
        tileEl.classList.add("selected");
      }
      tileEl.addEventListener("click", () => {
        selectedTileId = selectedTileId === tile.id ? null : tile.id;
        selectedTileSource = selectedTileId ? "hand" : null;
        updateButtons();
        renderSouthHand();
        renderDrawSlot();
      });
    }
    southHandEl.appendChild(tileEl);
  });
}

function renderOpponents() {
  renderBackTiles(northHandEl, aiHands.north.length, "flat");
  renderBackTiles(eastHandEl, aiHands.east.length, "col");
  renderBackTiles(westHandEl, aiHands.west.length, "col");
}

function renderBackTiles(container, count, variant) {
  container.innerHTML = "";
  const frag = document.createDocumentFragment();
  for (let i = 0; i < count; i++) {
    const tileEl = document.createElement("div");
    tileEl.className = "tile back" + (variant === "col" ? " slim" : variant === "flat" ? " flat" : "");
    tileEl.textContent = "Maj";
    frag.appendChild(tileEl);
  }
  container.appendChild(frag);
}

function renderDiscards() {
  Object.keys(discards).forEach(seat => {
    const pile = discards[seat];
    const el = discardEls[seat];
    el.innerHTML = "";
    pile.forEach(tile => {
      const tileEl = document.createElement("div");
      tileEl.className = "tile discard-tile";
      tileEl.textContent = tile.code;
      el.appendChild(tileEl);
    });
  });
}

function renderRemaining() {
  remainingEl.textContent = remainingTiles;
}

function renderDora() {
  doraTilesEl.innerHTML = "";
  doraIndicators.forEach((tile, idx) => {
    const tileEl = document.createElement("div");
    tileEl.className = "tile";
    tileEl.textContent = idx === 0 ? tile.code : "";
    if (idx > 0) tileEl.classList.add("back");
    doraTilesEl.appendChild(tileEl);
  });
}

function updateButtons() {
  discardBtn.disabled = !canDiscard || !selectedTileId;
  winBtn.disabled = totalTileCount() !== 13 || canDiscard || drawnTile !== null;
}

// --- Event handlers ---
drawBtn.addEventListener("click", () => {
  if (canDiscard) return;
  if (!deck.length) return;
  const [tile] = drawMany(1);
  if (!tile) return;
  drawnTile = tile;
  canDiscard = true;
  selectedTileId = null;
  selectedTileSource = null;
  refreshScores();
  renderAll();
});

discardBtn.addEventListener("click", () => {
  handleDiscardSelected();
});

function handleDiscardSelected() {
  if (!canDiscard || !selectedTileId) return;
  let discardedTile = null;
  if (selectedTileSource === "draw" && drawnTile) {
    discardedTile = drawnTile;
    drawnTile = null;
  } else {
    const index = playerHand.findIndex(t => t.id === selectedTileId);
    if (index === -1) return;
    const [tile] = playerHand.splice(index, 1);
    discardedTile = tile;
    if (drawnTile) {
      playerHand.push(drawnTile);
      drawnTile = null;
    }
  }
  discards.south.push(discardedTile);
  canDiscard = false;
  selectedTileId = null;
  selectedTileSource = null;
  refreshScores();
  renderAll();
  triggerAiDiscards();
}

winBtn.addEventListener("click", async () => {
  if (winBtn.disabled) return;
  const canWin = await checkWinCondition(playerHand);
  if (canWin) {
    openWinModal(getFullTilesForWin());
  }
});

closeModalBtn.addEventListener("click", () => {
  modalOverlay.classList.remove("show");
  initGame();
});
continueBtn.addEventListener("click", () => {
  modalOverlay.classList.remove("show");
  initGame();
});

function openWinModal(hand) {
  modalHandEl.innerHTML = "";
  const sorted = sortTiles(hand);
  sorted.forEach(tile => {
    const tileEl = document.createElement("div");
    tileEl.className = "tile";
    tileEl.textContent = tile.code;
    modalHandEl.appendChild(tileEl);
  });
  fetchWinSummary().then(summary => {
    modalSummaryEl.textContent = summary;
  });
  modalOverlay.classList.add("show");
}

// --- AI discard simulation ---
async function triggerAiDiscards() {
  const order = ["east", "north", "west"];
  for (let i = 0; i < order.length; i++) {
    if (gameOver) break;
    await processAiTurn(order[i]);
  }
}

async function processAiTurn(seat) {
  if (!deck.length || gameOver) return;
  const tile = aiDiscard(seat);
  aiTurnCounter += 1;
  const options = await fetchActionOptions(aiTurnCounter);
  const hasAction = options.canChi || options.canPeng || options.canRong;
  if (!hasAction) {
    await delay(500);
    return;
  }
  await promptAction(options, seat, tile);
}

function aiDiscard(seat) {
  const [tile] = drawMany(1);
  if (!tile) return null;
  discards[seat].push(tile);
  renderDiscards();
  renderRemaining();
  return tile;
}

function promptAction(options, seat, tile) {
  showActionButtons(options);
  pendingActionContext = { seat, tile };
  return new Promise(resolve => {
    pendingActionResolve = () => {
      pendingActionResolve = null;
      pendingActionContext = null;
      resolve();
    };
  });
}

function showActionButtons(options) {
  Object.values(actionBtns).forEach(btn => {
    btn.classList.add("hidden");
  });
  if (options.canChi) actionBtns.chi.classList.remove("hidden");
  if (options.canPeng) actionBtns.peng.classList.remove("hidden");
  if (options.canRong) actionBtns.rong.classList.remove("hidden");
  actionBtns.skip.classList.remove("hidden");
}

function hideActionButtons() {
  Object.values(actionBtns).forEach(btn => btn.classList.add("hidden"));
}

function handleActionChoice(action) {
  if (!pendingActionResolve || !pendingActionContext) return;
  const { seat, tile } = pendingActionContext;
  if (action === "skip") {
    hideActionButtons();
    delay(500).then(pendingActionResolve);
    return;
  }
  if (action === "rong") {
    hideActionButtons();
    gameOver = true;
    openWinModal(getFullTilesForWin());
    pendingActionResolve();
    return;
  }
  if (action === "chi" || action === "peng") {
    performMeld(seat, tile);
    hideActionButtons();
    delay(500).then(pendingActionResolve);
  }
}

function performMeld(seat, tile) {
  const pile = discards[seat];
  // remove the last tile from that seat to meld with
  if (pile.length) {
    pile.pop();
  }
  const chosen = [];
  for (let i = 0; i < 2; i++) {
    if (playerHand.length) {
      const idx = Math.floor(Math.random() * playerHand.length);
      chosen.push(playerHand.splice(idx, 1)[0]);
    } else if (drawnTile) {
      chosen.push(drawnTile);
      drawnTile = null;
    }
  }
  const meldTiles = [tile, ...chosen].filter(Boolean);
  melds.push(meldTiles);
  selectedTileId = null;
  selectedTileSource = null;
  // After chi/peng, player has +1 tile overall; must discard once.
  canDiscard = true;
  refreshScores();
  renderAll();
}

// --- Render helpers ---
function renderMelds() {
  meldsEl.innerHTML = "";
  melds.forEach(group => {
    const groupEl = document.createElement("div");
    groupEl.className = "meld-group";
    group.forEach(tile => {
      const tileEl = document.createElement("div");
      tileEl.className = "tile meld-tile";
      tileEl.textContent = tile.code;
      groupEl.appendChild(tileEl);
    });
    meldsEl.appendChild(groupEl);
  });
}

function renderDrawSlot() {
  drawSlotEl.innerHTML = "";
  if (!drawnTile) return;
  const tileEl = document.createElement("div");
  tileEl.className = "tile drawn";
  tileEl.textContent = drawnTile.code;
  const scoreEl = document.createElement("div");
  scoreEl.className = "score";
  scoreEl.textContent = tileScores[drawnTile.id] !== undefined ? tileScores[drawnTile.id] : "-";
  tileEl.appendChild(scoreEl);
  if (canDiscard) {
    tileEl.classList.add("playable");
    if (selectedTileId === drawnTile.id) {
      tileEl.classList.add("selected");
    }
    tileEl.addEventListener("click", () => {
      const alreadySelected = selectedTileId === drawnTile.id;
      selectedTileId = alreadySelected ? null : drawnTile.id;
      selectedTileSource = selectedTileId ? "draw" : null;
      updateButtons();
      renderDrawSlot();
    });
  }
  drawSlotEl.appendChild(tileEl);
}

function delay(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

function totalTileCount() {
  const meldCount = melds.reduce((sum, m) => sum + m.length, 0);
  return playerHand.length + meldCount + (drawnTile ? 1 : 0);
}

function getFullTilesForWin() {
  const meldTiles = melds.flat();
  const draw = drawnTile ? [drawnTile] : [];
  return [...playerHand, ...draw, ...meldTiles];
}

// --- wire action buttons ---
Object.entries(actionBtns).forEach(([key, btn]) => {
  if (!btn) return;
  btn.addEventListener("click", () => handleActionChoice(key));
});
