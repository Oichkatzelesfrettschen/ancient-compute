<script lang="ts">
    import Dial from './Dial.svelte';

    export let registers: Record<string, any> = {};
    export let title: string = "Registers";
    export let useDials: boolean = false;
</script>

<div class="register-view">
    <h3>{title}</h3>
    <div class="grid">
        {#each Object.entries(registers) as [name, value]}
            <div class="register">
                {#if useDials && typeof value === 'number'}
                    <Dial {value} label={name} />
                {:else}
                    <span class="label">{name}:</span>
                    <span class="value">{value}</span>
                {/if}
            </div>
        {/each}
    </div>
</div>

<style>
    .register-view {
        border: 1px solid #ddd;
        padding: 1rem;
        background: white;
        border-radius: 4px;
    }
    
    .grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(100px, 1fr));
        gap: 1rem;
        align-items: end;
    }
    
    .register {
        display: flex;
        flex-direction: column;
        align-items: center;
        padding: 0.5rem;
        background: #f9f9f9;
        font-family: monospace;
        border-radius: 4px;
    }
    
    .register:not(:has(.dial-container)) {
        flex-direction: row;
        justify-content: space-between;
    }
    
    .label {
        font-weight: bold;
        color: #555;
        font-size: 0.8rem;
    }
</style>