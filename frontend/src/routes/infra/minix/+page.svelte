<script lang="ts">
  import { onMount } from 'svelte';

  let arch = 'i386';
  let metrics: any = null;
  let runs: any[] = [];
  let summary: any = null;
  let error = '';
  let csvUrl = '';
  let series: { t: Date; cpu: number; rss: number; dRead: number; dWrite: number }[] = [];
  let selectedRun: string = '';

  async function fetchJson(path: string) {
    const res = await fetch(path);
    if (!res.ok) throw new Error(`${res.status} ${res.statusText}`);
    return res.json();
  }

  async function loadCsv(url: string) {
    const res = await fetch(url);
    if (!res.ok) return [];
    const text = await res.text();
    const rows = text.trim().split(/\r?\n/);
    const out: any[] = [];
    for (let i = 1; i < rows.length; i++) {
      const cols = rows[i].split(',');
      if (cols.length < 7) continue;
      const t = new Date(cols[0]);
      const cpu = parseFloat(cols[1] || '0');
      const rss = (parseFloat(cols[2] || '0')) / 1024.0; // MB
      const dRead = parseFloat(cols[5] || '0');
      const dWrite = parseFloat(cols[6] || '0');
      out.push({ t, cpu, rss, dRead, dWrite });
    }
    return out;
  }

  async function loadAll() {
    try {
      const base = '/api/v1/infra/minix';
      const m = await fetchJson(`${base}/metrics?arch=${arch}`);
      metrics = m;
      const r = await fetchJson(`${base}/runs?arch=${arch}`);
      runs = r.runs || [];
      const s = await fetchJson(`${base}/summary?arch=${arch}`);
      summary = s?.summary || null;
      csvUrl = `${base}/resource?arch=${arch}`;
      series = await loadCsv(csvUrl);
    } catch (e: any) {
      error = e?.message || 'Failed to load metrics';
    }
  }

  function fmt(n: number | null | undefined, suffix = '') {
    if (n == null) return '—';
    return `${n}${suffix}`;
  }

  async function selectRun(runId: string) {
    selectedRun = runId;
    const base = '/api/v1/infra/minix';
    csvUrl = `${base}/run/${encodeURIComponent(runId)}/resource?arch=${arch}`;
    series = await loadCsv(csvUrl);
  }

  onMount(loadAll);
</script>

<svelte:head>
  <title>MINIX Metrics</title>
</svelte:head>

<h1>MINIX Metrics ({arch})</h1>

{#if error}
  <p style="color: var(--color-error, red)">{error}</p>
{/if}

{#if metrics?.available}
  <section>
    <h2>Latest Boot</h2>
    <pre>{JSON.stringify(metrics.metrics, null, 2)}</pre>
  </section>
{:else}
  <p>No metrics available yet.</p>
{/if}

<section>
  <h2>Summary</h2>
  {#if summary}
  <ul>
    <li>Runs: {fmt(summary.run_count)}</li>
    <li>Average Boot: {fmt(summary.avg_ms, ' ms')}</li>
    <li>Min Boot: {fmt(summary.min_ms, ' ms')}</li>
    <li>Max Boot: {fmt(summary.max_ms, ' ms')}</li>
  </ul>
  <div class="chart">
    <h3>Boot Time Trend (recent runs)</h3>
    {@html lineChart((summary.recent || []).map((r:any) => r.boot_duration_ms), { color: '#7b61ff' })}
  </div>
  {:else}
  <p>No summary available.</p>
  {/if}
</section>

<section>
  <h2>Recent Runs</h2>
  {#if runs.length === 0}
    <p>No runs logged.</p>
  {:else}
    <table>
      <thead>
        <tr><th>Run ID</th><th>Timestamp</th><th>Boot Time (ms)</th></tr>
      </thead>
      <tbody>
        {#each runs as r}
          <tr>
            <td><button class="linklike" on:click={() => selectRun(r.runId)}>{r.runId}</button></td>
            <td>{r.timestamp}</td>
            <td>{r.bootDurationMs}</td>
          </tr>
        {/each}
      </tbody>
    </table>
  {/if}
  <p style="margin-top: 1rem; font-size: 0.9em; opacity: 0.8">
    CSV and logs available under <code>metrics/minix/{arch}/</code> in the repo.
  </p>
</section>

<section>
  <h2>Resource Timeseries</h2>
  {#if series.length === 0}
    <p>No timeseries available.</p>
  {:else}
    <p><a class="btn" href={csvUrl} download>Download CSV</a></p>
    <div class="chart-row">
      <div class="chart">
        <h3>CPU %</h3>
        {@html lineChart(series.map(s => s.cpu), { color: '#4a90e2' })}
      </div>
      <div class="chart">
        <h3>RSS (MB)</h3>
        {@html lineChart(series.map(s => s.rss), { color: '#50e3c2' })}
      </div>
      <div class="chart">
        <h3>IO Δ (bytes/s)</h3>
        {@html lineChart(series.map(s => s.dRead), { color: '#f5a623', label: 'read' })}
        {@html lineChart(series.map(s => s.dWrite), { color: '#d0021b', overlay: true, label: 'write' })}
      </div>
    </div>
  {/if}
</section>

<style>
  h1, h2 { margin: 0.6rem 0; }
  table { border-collapse: collapse; width: 100%; }
  th, td { border: 1px solid #ddd; padding: 0.5rem; text-align: left; }
  thead { background: #f4f4f4; }
  .linklike { background: none; border: none; color: #337ab7; cursor: pointer; }
  .linklike:hover { text-decoration: underline; }
  .chart-row { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 1rem; }
  .chart { background: #fff; border: 1px solid #eee; padding: 0.5rem; }
  .btn { display: inline-block; padding: 0.3rem 0.6rem; background: #666; color: #fff; text-decoration: none; border-radius: 4px; }
  .btn:hover { background: #555; }
</style>

<script lang="ts">
  // Minimal inline SVG line chart renderer
  export function lineChart(values: number[], opts: { color?: string; overlay?: boolean; label?: string } = {}): string {
    const w = 320, h = 120, pad = 6;
    if (!values || values.length === 0) return '<svg width="'+w+'" height="'+h+'"></svg>';
    const minV = Math.min(...values);
    const maxV = Math.max(...values, minV + 1);
    const range = maxV - minV || 1;
    const step = (w - pad * 2) / Math.max(1, values.length - 1);
    const pts = values.map((v, i) => {
      const x = pad + i * step;
      const y = h - pad - ((v - minV) / range) * (h - pad * 2);
      return `${x.toFixed(1)},${y.toFixed(1)}`;
    }).join(' ');
    const stroke = opts.color || '#4a90e2';
    const polyline = `<polyline fill="none" stroke="${stroke}" stroke-width="1.5" points="${pts}"/>`;
    const label = opts.label ? `<text x="${pad}" y="${pad*3}" font-size="10" fill="${stroke}">${opts.label}</text>` : '';
    const svg = `<svg width="${w}" height="${h}" viewBox="0 0 ${w} ${h}">${label}${polyline}</svg>`;
    return opts.overlay ? svg.replace('<svg','<svg style="position:relative;top:-122px"') : svg;
  }
</script>
