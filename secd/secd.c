#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

enum Op
{
    OP_NIL = 0, // Empty list
    OP_LD,      // Push value from environment to stack
    OP_LDC,     // Push constant to stack
    OP_LDF,     // Push combinator to stack

    OP_AP,      // Apply function
    OP_RTN,     // Push result on restored stack

    OP_DUM,     // Dummy value
    OP_RAP,     // Like AP but supports recursive function calls

    OP_SEL,     // Conditional branching
    OP_JOIN,    // Jump to label

    OP_CAR,     // Pops pair and pushes first element
    OP_CDR,     // Pops pair and pushes second element
    OP_ATOM,    // Push true if value is atomic
    OP_CONS,    // Pop two values from stack and push a pair

    OP_ADD,
    OP_EQ,

    OP_STOP     // End execution
};

typedef struct LL {
    void* data;
    struct LL* next;
} LL;

typedef struct {
    char *name;
    void* value;
} Map;

typedef struct {
    enum Op op;
    void* arg1;
    void* arg2;
} Instr;

typedef struct {
    size_t capacity;
    size_t index;
    Map *data[1024]; // 1024 is here directly link with the value passed in `new_state`
} Hashtbl;

typedef struct {
    size_t len;
    int index;
    Instr **lst;
} Array;

typedef struct State {
    LL *stack;
    Hashtbl *env;
    Array *input;
    struct State *dump;
} State;

typedef struct {
    char *name;
    void *body;
    Hashtbl *env;
} Closure;

Hashtbl*
new_hashtbl(int capacity)
{
    Hashtbl *hashtbl = malloc(sizeof(Hashtbl));
    if (!hashtbl) {
        perror("Failed to allocate hastbl");
        return NULL;
    }
    hashtbl->capacity = capacity;
    hashtbl->index = 0;
    return hashtbl;
}

State*
new_state(Instr **lst,
          size_t len)
{
    State *state = malloc(sizeof(State));
    if (!state) {
        perror("Failed to allocate State");
        return NULL;
    }
    state->stack = NULL;
    state->env = new_hashtbl(1024);
    Array *input = malloc(sizeof(Array));
    if (!input) {
        perror("Failed to allocate Array");
        free(state);
        return NULL;
    }
    input->len = len;
    input->index = 0;
    input->lst = lst;
    state->input = input;
    state->dump = NULL;
    return state;
}

Instr*
next(Array *arr)
{
    if (arr && arr->index < arr->len)
        return arr->lst[arr->index++];
    return NULL;
}

void
push_stack(State* state, void *value)
{
    LL* new_node = malloc(sizeof(LL));
    if (!new_node) {
        perror("Failed to allocate LL");
        return;
    }
    new_node->data = value;
    new_node->next = state->stack;
    state->stack = new_node;
}

Map*
new_map(char *name, void *value)
{
    Map *map = malloc(sizeof(Map));
    if (!map) {
        perror("Failed to allocate Map");
        return NULL;
    }
    map->name = name;
    map->value = value;
    return map;
}

// TODO: Add a hashtbl.(c/h) with utils functions
void
add_entry(State* state,
          char* name,
          void* value)
{
    if (state->env && state->env->index < state->env->capacity) {
        state->env->data[state->env->index] = new_map(name, value);
        state->env->index++;
    }
    fprintf(stderr, "Failed to add entry");
}

/*
 * LDF 	s e (LDF f.c) d            ->  ((f.e).s) e c d
 * AP  	((f.e') v.s) e (AP.c) d    ->  NIL (v.e') f (s e c.d)
 * RTN	(x.z) e' (RTN.q) (s e c.d) ->  (x.s) e c d
 */
void
vm(State* state)
{
    while (state->input->lst[state->input->index] != (void *)OP_STOP) {
        Instr* instr = next(state->input);

        // SECD abstract machine
        if (instr == (void*)OP_LD) {
            push_stack(state, instr->arg1);
        } else if (instr == (void*)OP_LDC) {
            push_stack(state, instr->arg1);
        } else if (instr == (void*)OP_LDF) {
            Closure *closure = malloc(sizeof(Closure));
            closure->name = instr->arg1;
            closure->body = instr->arg2;
            closure->env = state->env;
            push_stack(state, closure);
        } else if (instr == (void*)OP_AP) {

        }
    }
}

Instr test1_0 = {OP_LDC, (void*)1, NULL };
Instr test1_1 = {OP_LDC, (void*)1, NULL };
Instr test1_2 = {OP_ADD, NULL, NULL };
Instr *test1[] = {&test1_0, &test1_1, &test1_2};

int
main(void)
{
    State* state = new_state(test1, sizeof(test1)/sizeof(test1[0]));
    return 0;
}
