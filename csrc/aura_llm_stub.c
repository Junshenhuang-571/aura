/* aura_llm_stub.c — default no-op LLM backend.
 *
 * When you build llm.f90 / llama.cpp support, provide real
 * aura_llm_available/aura_llm_generate in another C file and drop this one
 * from the build (they would be duplicate symbols otherwise).
 */
#include <stddef.h>

int aura_llm_available(void) { return 0; }

const char* aura_llm_generate(const char* prompt, const char* context)
{
    (void)prompt; (void)context;
    return "";
}
