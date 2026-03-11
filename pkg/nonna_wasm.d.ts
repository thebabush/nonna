/* tslint:disable */
/* eslint-disable */

export function analyze_source(filename: string, source_bytes: Uint8Array): any;

export function clear_uploaded(): any;

export function compare(a: number, b: number, weights_json: string): any;

export function get_function(id: number): any;

export function get_packages(): any;

export function get_pairs(params_json: string): any;

export function get_stats(): any;

export function init_panic_hook(): void;

export function load_corpus(data: Uint8Array): any;

export function search(query: string): any;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
    readonly memory: WebAssembly.Memory;
    readonly analyze_source: (a: number, b: number, c: number, d: number) => [number, number, number];
    readonly clear_uploaded: () => [number, number, number];
    readonly compare: (a: number, b: number, c: number, d: number) => [number, number, number];
    readonly get_function: (a: number) => [number, number, number];
    readonly get_packages: () => [number, number, number];
    readonly get_pairs: (a: number, b: number) => [number, number, number];
    readonly get_stats: () => [number, number, number];
    readonly load_corpus: (a: number, b: number) => [number, number, number];
    readonly search: (a: number, b: number) => [number, number, number];
    readonly init_panic_hook: () => void;
    readonly __wbindgen_free: (a: number, b: number, c: number) => void;
    readonly __wbindgen_malloc: (a: number, b: number) => number;
    readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
    readonly __wbindgen_externrefs: WebAssembly.Table;
    readonly __externref_table_dealloc: (a: number) => void;
    readonly __wbindgen_start: () => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;

/**
 * Instantiates the given `module`, which can either be bytes or
 * a precompiled `WebAssembly.Module`.
 *
 * @param {{ module: SyncInitInput }} module - Passing `SyncInitInput` directly is deprecated.
 *
 * @returns {InitOutput}
 */
export function initSync(module: { module: SyncInitInput } | SyncInitInput): InitOutput;

/**
 * If `module_or_path` is {RequestInfo} or {URL}, makes a request and
 * for everything else, calls `WebAssembly.instantiate` directly.
 *
 * @param {{ module_or_path: InitInput | Promise<InitInput> }} module_or_path - Passing `InitInput` directly is deprecated.
 *
 * @returns {Promise<InitOutput>}
 */
export default function __wbg_init (module_or_path?: { module_or_path: InitInput | Promise<InitInput> } | InitInput | Promise<InitInput>): Promise<InitOutput>;
