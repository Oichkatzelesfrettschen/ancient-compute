<script lang="ts">
    import { createEventDispatcher } from 'svelte';
    
    const dispatch = createEventDispatcher();
    
    export let isPaused: boolean = true;
    
    function onStep() {
        dispatch('step');
    }
    
    function onContinue() {
        dispatch('continue');
    }
    
    function onReset() {
        dispatch('reset');
    }
</script>

<div class="controls">
    <button on:click={onStep} disabled={!isPaused}>Step</button>
    <button on:click={onContinue} disabled={!isPaused}>Continue</button>
    <button on:click={onReset} class="reset">Reset</button>
</div>

<style>
    .controls {
        display: flex;
        gap: 1rem;
        padding: 1rem;
        background: #eee;
        border-bottom: 1px solid #ccc;
    }
    
    button {
        padding: 0.5rem 1rem;
        cursor: pointer;
    }
    
    .reset {
        margin-left: auto;
        background: #fee;
        border-color: #faa;
    }
</style>
