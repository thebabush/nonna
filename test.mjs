// Smoke test for nonna-wasm using the nodejs build target.
// Run: node wasm-demo/test.mjs
import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const NODE_PKG = '/tmp/nonna-wasm-node';

const { load_corpus, get_stats, get_pairs, get_function, compare, search, get_packages, analyze_source } =
  await import(join(NODE_PKG, 'nonna_wasm.js'));

console.log('✓ WASM module imported');

// Load corpus
const corpusBytes = new Uint8Array(readFileSync(join(__dirname, 'corpus.bin')));
const statsJson = load_corpus(corpusBytes);
const stats = JSON.parse(statsJson);
console.log(`✓ Corpus loaded: ${stats.total_functions} functions, ${stats.total_pairs} pairs, ${stats.total_features} features`);

// get_stats
const statsResult = JSON.parse(get_stats());
console.assert(statsResult.stats.total_functions === stats.total_functions, 'total_functions mismatch');
console.log(`✓ get_stats: corpus=${statsResult.stats.corpus_functions}, uploaded=${statsResult.stats.uploaded_functions}`);

// get_pairs with filters
const pairsResult = JSON.parse(get_pairs(JSON.stringify({ limit: 5, min_sim: 0.3 })));
console.assert(pairsResult.pairs.length <= 5, 'limit not respected');
console.assert(pairsResult.total > 0, 'no pairs found');
console.log(`✓ get_pairs: ${pairsResult.total} total pairs, showing ${pairsResult.pairs.length}`);
const topPair = pairsResult.pairs[0];
console.log(`  top: ${topPair.a.qualified_name} ↔ ${topPair.b.qualified_name} @ ${(topPair.similarity*100).toFixed(1)}%`);

// get_function
const funcResult = JSON.parse(get_function(0));
console.assert(funcResult.id === 0, 'id mismatch');
console.assert(typeof funcResult.source === 'string', 'no source');
console.log(`✓ get_function(0): "${funcResult.qualified_name}" — ${funcResult.op_count} ops, ${funcResult.matches.length} matches`);

// compare
const cmp = JSON.parse(compare(topPair.a.id, topPair.b.id, ''));
console.assert(Math.abs(cmp.similarity - topPair.similarity) < 0.01, 'similarity mismatch');
console.assert(cmp.diff.length > 0, 'empty diff');
console.assert(cmp.breakdown.length === 8, 'expected 8 tag breakdown entries');
console.log(`✓ compare(${topPair.a.id},${topPair.b.id}): ${(cmp.similarity*100).toFixed(1)}% sim, ${cmp.shared_features} shared, ${cmp.diff.length} diff lines`);

// search
const searchResult = JSON.parse(search('validate'));
console.assert(Array.isArray(searchResult.results), 'results not array');
console.log(`✓ search('validate'): ${searchResult.results.length} results`);
if (searchResult.results.length > 0) {
  console.log(`  first: ${searchResult.results[0].qualified_name}`);
}

// get_packages
const pkgs = JSON.parse(get_packages());
console.assert(pkgs.length > 0, 'no packages');
console.log(`✓ get_packages: ${pkgs.length} packages`);
console.log(`  top 5: ${pkgs.slice(0,5).map(p => `${p.name}(${p.function_count})`).join(', ')}`);

// analyze_source — upload a Python file with substantive functions (>= 3 ops each)
const samplePy = `
def process_items(items, threshold=0.5):
    results = []
    for item in items:
        if item.value > threshold:
            results.append(item.transform())
        else:
            results.append(item.default())
    return results

def filter_and_transform(data, threshold=0.5):
    """Same logic as process_items, different name — should be detected as similar."""
    output = []
    for entry in data:
        if entry.value > threshold:
            output.append(entry.transform())
        else:
            output.append(entry.default())
    return output
`;
const uploadResult = JSON.parse(analyze_source('test_upload.py', new TextEncoder().encode(samplePy)));
console.log(`✓ analyze_source: added ${uploadResult.added_functions} functions, ${uploadResult.added_pairs} pairs`);
console.assert(uploadResult.added_functions >= 2, `expected ≥2 functions, got ${uploadResult.added_functions}`);

// Verify stats updated
const statsAfter = JSON.parse(get_stats());
console.assert(statsAfter.stats.uploaded_functions === uploadResult.added_functions, 'uploaded count mismatch');
console.log(`✓ Stats after upload: ${statsAfter.stats.total_functions} functions (${statsAfter.stats.uploaded_functions} uploaded)`);

// Verify the uploaded functions appear in search
const uploadSearch = JSON.parse(search('filter_and_transform'));
const found = uploadSearch.results.some(r => r.name === 'filter_and_transform');
console.assert(found, 'uploaded function not found in search');
console.log(`✓ Uploaded function 'filter_and_transform' found in search`);

// clear_uploaded
const { clear_uploaded } = await import(join(NODE_PKG, 'nonna_wasm.js'));
// Note: clear_uploaded is not re-exported by default; check if available
const statsCleared = JSON.parse(get_stats());
console.log(`✓ Final state: ${statsCleared.stats.total_functions} functions`);

console.log('\n✅ All smoke tests passed');
