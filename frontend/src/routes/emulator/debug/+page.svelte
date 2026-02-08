<script lang="ts">
    import { onMount } from 'svelte';
    import { toolsApi, type DebugState, type PerformanceReport } from '$lib/api/tools';
    import DebuggerControls from '$lib/components/emulator/DebuggerControls.svelte';
    import RegisterView from '$lib/components/emulator/RegisterView.svelte';
    import PerformanceView from '$lib/components/emulator/PerformanceView.svelte';

    let state: DebugState | null = null;
    let perfReport: PerformanceReport | null = null;
    let isPaused = true;
    let error: string | null = null;

    async function step() {
        try {
            const res = await toolsApi.step();
            state = res.state;
            if (res.triggered_breakpoints) {
                console.log("Breakpoints hit:", res.triggered_breakpoints);
                isPaused = true;
            }
            await updatePerf();
        } catch (e) {
            error = String(e);
        }
    }

    async function continueExec() {
        try {
            const res = await toolsApi.command('continue');
            await step();
        } catch (e) {
            error = String(e);
        }
    }

    async function reset() {
        try {
            await toolsApi.command('reset');
            await step();
            if (state) state.cycle = 0;
        } catch (e) {
            error = String(e);
        }
    }

    async function updatePerf() {
        try {
            perfReport = await toolsApi.getPerformance();
        } catch (e) {
            console.error("Failed to fetch performance report", e);
        }
    }

    onMount(async () => {
        await reset();
    });
</script>

<div class="debugger-page">
    <h1>Unified Engine Debugger</h1>
    
    {#if error}
        <div class="error">{error}</div>
    {/if}

    <DebuggerControls {isPaused} on:step={step} on:continue={continueExec} on:reset={reset} />

    <div class="main-layout">
        <div class="left-panel">
            {#if state}
                <div class="status-bar">
                    <span>Cycle: {state.cycle}</span>
                    <span>Phase: {state.phase || 'N/A'}</span>
                </div>
                
                <!-- Main Registers (Generic) -->
                <RegisterView registers={state.registers} title="Machine State" />
                
                <!-- Curta Specific Visualization -->
                {#if state.snapshot && state.snapshot.sliders}
                     <div class="curta-panel">
                         <RegisterView 
                            registers={state.snapshot.sliders} 
                            title="Curta Sliders (Input)" 
                            useDials={true} 
                         />
                         
                         <!-- Split Result into digits for dial view? For now, raw value -->
                         <div class="dials-row">
                             <div class="dial-group">
                                 <h4>Result Dial</h4>
                                 <div class="digital-display">{state.snapshot.result}</div>
                             </div>
                             <div class="dial-group">
                                 <h4>Counter Dial</h4>
                                 <div class="digital-display">{state.snapshot.counter}</div>
                             </div>
                         </div>
                     </div>
                {/if}

                <!-- Analytical Engine Specific Visualization -->
                {#if state.snapshot && state.snapshot.barrel}
                    <div class="ae-panel">
                        <h3>Analytical Engine Micro-State</h3>
                        <p>Active Barrel: {state.snapshot.barrel.active || 'Idle'}</p>
                        <p>Barrel Step: {state.snapshot.barrel.step}</p>
                        <p>Mill Operand Buffer: {state.snapshot.mill_operand_buffer}</p>
                        <p>Mill Result Buffer: {state.snapshot.mill_result_buffer}</p>
                        <p>Active Store Address: {state.snapshot.active_store_address !== null ? state.snapshot.active_store_address : 'N/A'}</p>
                    </div>
                {/if}
            {:else}
                <p>Loading engine state...</p>
            {/if}
        </div>
        
        <div class="right-panel">
            <PerformanceView report={perfReport} />
        </div>
    </div>
</div>

<style>
    .debugger-page {
        font-family: sans-serif;
        max-width: 1200px;
        margin: 0 auto;
    }
    
    .error {
        background: #fdd;
        color: #c00;
        padding: 1rem;
        margin: 1rem;
    }
    
    .main-layout {
        display: grid;
        grid-template-columns: 2fr 1fr;
        gap: 1rem;
        padding: 1rem;
    }
    
    .status-bar {
        background: #333;
        color: #fff;
        padding: 0.5rem;
        margin-bottom: 1rem;
        display: flex;
        gap: 2rem;
        border-radius: 4px;
    }
    
    .curta-panel {
        margin-top: 1rem;
        border: 1px solid #ccc;
        padding: 1rem;
        background: #f4f4f4;
        border-radius: 4px;
    }

    .ae-panel {
        margin-top: 1rem;
        border: 1px solid #c9c9ff; /* Light purple border */
        padding: 1rem;
        background: #e0e0ff; /* Light purple background */
        border-radius: 4px;
    }
    
    .dials-row {
        display: flex;
        gap: 2rem;
        margin-top: 1rem;
    }
    
    .digital-display {
        font-family: 'Courier New', monospace;
        background: #222;
        color: #0f0;
        padding: 0.5rem 1rem;
        font-size: 1.5rem;
        border-radius: 2px;
        border: 2px solid #555;
    }
</style>