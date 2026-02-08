<script lang="ts">
    export let value: number = 0;
    export let label: string = "";
    
    // Calculate rotation: 0 = 0deg, 9 = 324deg (36deg per digit)
    // Or just display the number clearly in a circle.
    // Mechanical dials usually rotate.
    $: rotation = value * 36; 
</script>

<div class="dial-container">
    <div class="dial" style="transform: rotate({rotation}deg);">
        <div class="marker"></div>
        <span class="value" style="transform: rotate({-rotation}deg);">{value}</span>
    </div>
    {#if label}
        <span class="label">{label}</span>
    {/if}
</div>

<style>
    .dial-container {
        display: flex;
        flex-direction: column;
        align-items: center;
        width: 60px;
    }
    
    .dial {
        width: 40px;
        height: 40px;
        border-radius: 50%;
        border: 2px solid #333;
        background: #fff;
        position: relative;
        display: flex;
        align-items: center;
        justify-content: center;
        transition: transform 0.3s ease-out;
        box-shadow: inset 0 0 5px rgba(0,0,0,0.2);
    }
    
    .marker {
        position: absolute;
        top: 2px;
        width: 2px;
        height: 5px;
        background: red;
    }
    
    .value {
        font-family: 'Courier New', monospace;
        font-weight: bold;
        font-size: 1.2rem;
    }
    
    .label {
        font-size: 0.7rem;
        margin-top: 0.25rem;
        color: #666;
        text-transform: uppercase;
    }
</style>
