// Inline function definition with missing semicolon. This code fails to parse because of antlr's automatic error correction. After parsing
// 'true', antlr looks for a semi and can't find one - it then tries to correct the situation by deleting the current tokem,  '}', and using
// then following semi. 
//
// We then end up being short a '}'.
//
package aemon{class Dude{public function runHorse(dude){var dude = function(a){return true}; }}}
