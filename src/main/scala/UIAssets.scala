object UIAssets:
  val tailwindCss: String =
    """
      |@theme { --color-arena: #07111f; }
      |body { background: radial-gradient(circle at 50% -20%, #243b67 0%, #0b1425 38%, #050913 100%); }
      |.board { perspective: 900px; box-shadow: 0 32px 80px rgba(0,0,0,.48), inset 0 1px 1px rgba(255,255,255,.25); }
      |.slot { box-shadow: inset 0 5px 10px rgba(0,0,0,.72), 0 1px 0 rgba(255,255,255,.14); }
      |.piece-jev { background: radial-gradient(circle at 34% 28%, #fff4a8 0%, #facc15 26%, #d97706 78%); box-shadow: inset -6px -8px 12px rgba(120,53,15,.35), 0 0 18px rgba(250,204,21,.25); }
      |.piece-llm { background: radial-gradient(circle at 34% 28%, #fecaca 0%, #f43f5e 28%, #9f1239 80%); box-shadow: inset -6px -8px 12px rgba(76,5,25,.4), 0 0 18px rgba(244,63,94,.28); }
      |.piece-empty { background: radial-gradient(circle at 50% 55%, #030712, #111827); }
      |@keyframes drop { 0% { transform: translateY(-440px) scale(.88); } 72% { transform: translateY(8px) scale(1.03); } 88% { transform: translateY(-5px); } 100% { transform: translateY(0) scale(1); } }
      |.drop { animation: drop .58s cubic-bezier(.23,.82,.38,1.08); position: relative; z-index: 10; }
      |.thinking-ring { animation: pulseRing 1.5s ease-in-out infinite; }
      |@keyframes pulseRing { 0%,100% { box-shadow: 0 0 0 0 rgba(129,140,248,.15); } 50% { box-shadow: 0 0 0 9px rgba(129,140,248,0); } }
      |""".stripMargin

  val clientScript: String =
    """
      |(function () {
      |  var form = document.getElementById('game-form');
      |  var model = document.getElementById('model');
      |  var board = document.getElementById('board');
      |  var errorBox = document.getElementById('error');
      |  var timer = null, clockFrame = null, game = null, previousMoveCount = 0;
      |
      |  function esc(value) { return String(value).replace(/[&<>"']/g, function(c) { return {'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]; }); }
      |  function seconds(ms) { return (ms / 1000).toFixed(1) + 's'; }
      |  function money(value) { return '$' + Number(value || 0).toFixed(6); }
      |  function terminal(status) { return status === 'Won' || status === 'Draw' || status === 'Failed' || status === 'Cancelled'; }
      |  function showError(message) { errorBox.textContent = message; errorBox.classList.remove('hidden'); }
      |  function clearError() { errorBox.classList.add('hidden'); errorBox.textContent = ''; }
      |
      |  function emptyBoard() {
      |    board.innerHTML = '';
      |    for (var i = 0; i < 42; i++) {
      |      var slot = document.createElement('div');
      |      slot.className = 'slot aspect-square rounded-full p-[7%]';
      |      slot.innerHTML = '<div class="piece-empty h-full w-full rounded-full"></div>';
      |      board.appendChild(slot);
      |    }
      |  }
      |
      |  function render(next) {
      |    var newMove = next.moves.length > previousMoveCount && next.moves.length > 0;
      |    var newest = newMove ? next.moves[next.moves.length - 1] : null;
      |    board.innerHTML = '';
      |    next.board.forEach(function(row, rowIndex) {
      |      row.forEach(function(cell, columnIndex) {
      |        var slot = document.createElement('div');
      |        slot.className = 'slot aspect-square rounded-full p-[7%]';
      |        var animate = newest && newest.row === rowIndex && newest.column === columnIndex ? ' drop' : '';
      |        slot.innerHTML = '<div class="piece-' + cell + animate + ' h-full w-full rounded-full"></div>';
      |        board.appendChild(slot);
      |      });
      |    });
      |    previousMoveCount = next.moves.length;
      |    document.getElementById('status').textContent = next.message;
      |    document.getElementById('model-label').textContent = next.modelLabel;
      |    document.getElementById('model-pricing').textContent = '$' + next.inputUsdPerMillion + ' input · $' + next.outputUsdPerMillion + ' output per 1M tokens';
      |    document.getElementById('bedrock-cost').textContent = money(next.bedrockCostUsd);
      |    document.getElementById('jev-cost').textContent = money(next.jevCostUsd);
      |    document.getElementById('move-count').textContent = next.moves.length + (next.moves.length === 1 ? ' move' : ' moves');
      |    document.getElementById('cancel').classList.toggle('hidden', next.status !== 'Thinking');
      |    document.getElementById('start').disabled = next.status === 'Thinking';
      |
      |    ['jev','llm'].forEach(function(name) {
      |      var active = next.currentPlayer && next.currentPlayer.toLowerCase() === name;
      |      document.getElementById(name + '-card').classList.toggle('thinking-ring', active);
      |      document.getElementById(name + '-card').classList.toggle('ring-2', active);
      |      document.getElementById(name + '-card').classList.toggle('ring-indigo-400/50', active);
      |      var moves = next.moves.filter(function(m) { return m.player.toLowerCase() === name; });
      |      var total = moves.reduce(function(sum, m) { return sum + m.durationMs; }, 0);
      |      var cost = name === 'llm' ? next.bedrockCostUsd : next.jevCostUsd;
      |      document.getElementById(name + '-time').textContent = moves.length + ' turns · ' + seconds(total) + ' · ' + money(cost);
      |    });
      |
      |    var history = document.getElementById('history');
      |    if (!next.moves.length) history.innerHTML = '<li class="rounded-xl border border-dashed border-white/10 p-4 text-center text-sm text-slate-600">Waiting for the opening move…</li>';
      |    else history.innerHTML = next.moves.slice().reverse().map(function(m) {
      |      var jev = m.player === 'Jev';
      |      var color = jev ? 'amber' : 'rose';
      |      var tokens = m.inputTokens + m.outputTokens;
      |      var usage = tokens ? tokens + ' tokens' : '';
      |      if (tokens || Number(m.estimatedCostUsd) > 0) usage += (usage ? ' · ' : '') + money(m.estimatedCostUsd);
      |      return '<li class="rounded-xl border border-white/5 bg-black/15 p-3">' +
      |        '<div class="flex items-center justify-between gap-2"><div class="flex items-center gap-2"><span class="h-3 w-3 rounded-full piece-' + (jev ? 'jev' : 'llm') + '"></span><strong class="text-sm">' + esc(m.player) + '</strong><span class="text-xs text-slate-600">#' + m.turn + '</span></div><span class="font-mono text-sm font-bold text-' + color + '-300">' + seconds(m.durationMs) + '</span></div>' +
      |        '<p class="mt-1 text-xs text-slate-400">Column ' + m.column + ' · row ' + m.row + '</p><p class="mt-1 line-clamp-3 text-xs text-slate-500">' + esc(m.note) + '</p><div class="mt-1 text-[10px] text-slate-600">' + usage + '</div></li>';
      |    }).join('');
      |
      |    var connection = document.getElementById('connection');
      |    connection.innerHTML = '<span class="h-2 w-2 rounded-full ' + (terminal(next.status) ? 'bg-slate-500' : 'bg-emerald-400 animate-pulse') + '"></span> ' + next.status;
      |    updateClock(next);
      |  }
      |
      |  function updateClock(next) {
      |    if (clockFrame) cancelAnimationFrame(clockFrame);
      |    var clock = document.getElementById('turn-clock');
      |    if (!next.turnStartedAtMs || next.status !== 'Thinking') { clock.classList.add('hidden'); return; }
      |    clock.classList.remove('hidden');
      |    function tick() { clock.textContent = seconds(Math.max(0, Date.now() - next.turnStartedAtMs)); clockFrame = requestAnimationFrame(tick); }
      |    tick();
      |  }
      |
      |  async function poll() {
      |    if (!game) return;
      |    try {
      |      var response = await fetch('/api/games/' + encodeURIComponent(game.id), {cache:'no-store'});
      |      if (!response.ok) throw new Error('Could not load game state');
      |      game = await response.json(); render(game);
      |      if (terminal(game.status)) { clearInterval(timer); timer = null; }
      |    } catch (e) { showError(e.message); }
      |  }
      |
      |  form.addEventListener('submit', async function(event) {
      |    event.preventDefault(); clearError();
      |    if (timer) clearInterval(timer);
      |    previousMoveCount = 0;
      |    document.getElementById('start').disabled = true;
      |    try {
      |      var response = await fetch('/api/games', {method:'POST', headers:{'content-type':'application/json'}, body:JSON.stringify({modelId:model.value, firstPlayer:document.getElementById('first-player').value})});
      |      var payload = await response.json();
      |      if (!response.ok) throw new Error(payload.error || 'Could not start game');
      |      game = payload; render(game); timer = setInterval(poll, 350); poll();
      |    } catch (e) { document.getElementById('start').disabled = false; showError(e.message); }
      |  });
      |  document.getElementById('cancel').addEventListener('click', async function() {
      |    if (!game) return;
      |    await fetch('/api/games/' + encodeURIComponent(game.id) + '/cancel', {method:'POST'}); poll();
      |  });
      |  emptyBoard();
      |})();
      |""".stripMargin
