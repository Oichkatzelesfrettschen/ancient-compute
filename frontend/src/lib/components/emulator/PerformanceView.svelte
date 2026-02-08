<script lang="ts">
    export let report: any;
</script>

<div class="perf-view">
    <h3>Performance Analysis</h3>
    {#if report}
        <div class="stat">
            <span>Total Cycles:</span> <strong>{report.total_cycles}</strong>
        </div>
        
        <h4>Hot Spots</h4>
        <ul>
            <li>Opcode: {report.hot_spots.opcode || 'None'}</li>
        </ul>
        
        {#if report.suggestions && report.suggestions.length > 0}
            <h4>Optimization Suggestions</h4>
            <ul class="suggestions">
                {#each report.suggestions as suggestion}
                    <li>{suggestion}</li>
                {/each}
            </ul>
        {/if}
    {:else}
        <p>No data available.</p>
    {/if}
</div>

<style>
    .perf-view {
        padding: 1rem;
        background: #f0f4f8;
        border: 1px solid #dbe2e8;
    }
    
    .suggestions {
        color: #d32f2f;
    }
</style>
