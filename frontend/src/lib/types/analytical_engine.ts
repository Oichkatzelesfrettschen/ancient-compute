export interface AnalyticalEngineSnapshot {
    pc: number;
    registers: { [key: string]: number };
    flags: { [key: string]: boolean };
    clock_time: number;
    barrel: {
        active: string | null;
        step: number;
    };
    mill_operand_buffer: number;
    mill_result_buffer: number;
    active_store_address: number | null;
}
