/* Definitions of target machine for GNU compiler for picoJava
   Copyright (C) 2003 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Contributed by Bernd Paysan (bernd.paysan@gmx.de).  */

extern int target_flags;

#define TARGET_VERSION  fputs ("(b16)", stderr);

/* Once GDB has been enhanced to deal with functions without frame
   pointers, we can change this to allow for elimination of
   the frame pointer in leaf functions.  */
#define TARGET_DEFAULT 0
#define TARGET_FLT_EVAL_METHOD 0

#define TARGET_SWITCHES							      \
{ 							      		      \
    { "", TARGET_DEFAULT, 0}}

#define TARGET_CPU_CPP_BUILTINS()				\
  do								\
    {								\
    }								\
  while(0)


/* Define this to change the optimizations performed by default.  */
#define OPTIMIZATION_OPTIONS(LEVEL,SIZE)                                \
 do {                                                                   \
   if (optimize)                                                        \
       flag_force_addr = 1;                                             \
 } while (0)

/* Target machine storage layout.  */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN  0

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN 1

/* Define this to set the endianness to use in libgcc2.c, which can
   not depend on target_flags.  */
#define LIBGCC2_WORDS_BIG_ENDIAN 1

/* Number of bits in an addressable storage unit.  */
#define BITS_PER_UNIT  8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD  16
#define MAX_BITS_PER_WORD 16

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD  2

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE  16

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY   16

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY  16

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY  16

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY  16

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT  16

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 16

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  ((TREE_CODE (EXP) == STRING_CST       \
    && (ALIGN) < FASTEST_ALIGNMENT)     \
    ? FASTEST_ALIGNMENT : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)             \
  (TREE_CODE (TYPE) == ARRAY_TYPE               \
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode    \
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Set this non-zero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1


/* Standard register usage.  */

/* Useful predicates.  */

#define FIRST_PSEUDO_REGISTER 128

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */

#define FIXED_REGISTERS                                                    \
 {                                                                         \
   1,1,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, \
   0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, \
   0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, \
   0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, }
                                                                          

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  

   We pretend that some standard registers are call clobbered so the
   exception handler code has somewhere to play.  */

#define CALL_USED_REGISTERS                                                \
 {                                                                         \
   1,1,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, \
   0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, \
   1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, \
   1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, }

#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) 1

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Define this if the program counter is overloaded on a register.  */
/* #define PC_REGNUM               PC_REG */
#define TOR_REGNUM 0

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 1

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 2

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 3

#define G4_REG 4
#define O0_REG 64

/* Register in which the static-chain is passed to a function.  */
/* #define STATIC_CHAIN_REGNUM     G4_REG */

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may be
   accessed via the stack pointer) in functions that seem suitable.  */
#define FRAME_POINTER_REQUIRED  0

/* This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.  */

#define ELIMINABLE_REGS {                                               \
     { FRAME_POINTER_REGNUM,            STACK_POINTER_REGNUM    },      \
     { ARG_POINTER_REGNUM,              STACK_POINTER_REGNUM    } }    
     
/* Given FROM and TO register numbers, say whether this elimination
   is allowed.  */
#define CAN_ELIMINATE(FROM, TO) 1

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  OFFSET =  (((FROM) == FRAME_POINTER_REGNUM) ? get_frame_size() : 0)

/* For b16 we have to save 2 bytes of information for a non local 
   jump.  */

#define STACK_SAVEAREA_MODE(x) 2

/* If the structure value address is not passed in a register, define
   `STRUCT_VALUE' as an expression returning an RTX for the place
   where the address is passed.  If it returns 0, the address is
   passed as an "invisible" first argument.  */
#define STRUCT_VALUE 0

/* A C expression which can inhibit the returning of certain function
   values in registers, based on the type of value.  A nonzero value
   says to return the function value in memory, just as large
   structures are always returned.  Here TYPE will be a C expression
   of type `tree', representing the data type of the value.

   Note that values of mode `BLKmode' must be explicitly handled by
   this macro.  Also, the option `-fpcc-struct-return' takes effect
   regardless of this macro.  On most systems, it is possible to
   leave the macro undefined; this causes a default definition to be
   used, whose value is the constant 1 for `BLKmode' values, and 0
   otherwise.

   Do not use this macro to indicate that structures and unions
   should always be returned in memory.  You should instead use
   `DEFAULT_PCC_STRUCT_RETURN' to indicate this.  */
#define RETURN_IN_MEMORY(TYPE) \
  ((TYPE_MODE (TYPE) == BLKmode) || int_size_in_bytes (TYPE) > 8)

/* Don't default to pcc-struct-return, because we have already specified
   exactly how to return structures in the RETURN_IN_MEMORY macro.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */

enum reg_class
{
  NO_REGS,
  STD_REGS,         /* Standard registers, on opstack.  */
  ALL_REGS,
  LIM_REG_CLASSES
};

#define GENERAL_REGS ALL_REGS
#define HARD_REGNO_RENAME_OK(SRC, TARGET) 1
#define HARD_REGNO_NREGS(REGNO, MODE) \
	((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)
#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

#define N_REG_CLASSES  ((int) LIM_REG_CLASSES)

/* Give names of register classes as strings for dump files.  */
#define REG_CLASS_NAMES   \
{                         \
  "NO_REGS",              \
  "STD_REGS",             \
  "ALL_REGS"              \
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS                                              \
{                                                                       \
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000 }, /* NO_REGS  */    \
  { 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff }, /* STD_REGS */    \
  { 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff }, /* ALL_REGS */    \
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) STD_REGS
        
/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS  STD_REGS
#define BASE_REG_CLASS   STD_REGS

/* Get reg_class from a letter such as appears in the machine
   description.  */

#define REG_CLASS_FROM_LETTER(C) \
   ( (C) == 'S' ? STD_REGS \
   : (C) == 'D' ? STD_REGS \
   : NO_REGS)

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   I: arithmetic operand -127..128, as used in inc.
   K: 0.
   */

#define CONST_OK_FOR_I(VALUE) \
 (((HOST_WIDE_INT)(VALUE))>= 0 && ((HOST_WIDE_INT)(VALUE)) <= 255)

#define CONST_OK_FOR_K(VALUE) ((VALUE)==0)

#define CONST_OK_FOR_LETTER_P(VALUE, C)         \
     ((C) == 'I' ? CONST_OK_FOR_I (VALUE)       \
    : (C) == 'K' ? CONST_OK_FOR_K (VALUE)       \
    : 0)

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) 0

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X, CLASS) (CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.

   With picoJava this is the size of MODE in words.  */

#define CLASS_MAX_NREGS(CLASS, MODE) \
     ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)


/* A C expression whose value is nonzero if pseudos that have been
   assigned to registers of class CLASS would likely be spilled
   because registers of CLASS are needed for spill registers.

   For picoJava, something that isn't an incoming argument or a normal
   register is going to be very hard to get at. */

#define CLASS_LIKELY_SPILLED_P(X) ((X) != STD_REGS)

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */

#define STACK_GROWS_DOWNWARD 1

/* Define this macro if successive arguments to a function occupy
   decreasing addresses on the stack.  */

#define ARGS_GROW_DOWNWARD 1

/*  Define this macro if the addresses of local variable slots are at
    negative offsets from the frame pointer.  */

#define FRAME_GROWS_DOWNWARD 1

/* Offset from the frame pointer to the first local variable slot to
   be allocated.  */

#define STARTING_FRAME_OFFSET  0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.  */

/* Don't define PUSH_ROUNDING, since the hardware doesn't do this.
   When PUSH_ROUNDING is not defined, PARM_BOUNDARY will cause gcc to
   do correct alignment.  */

#define PUSH_ROUNDING(NPUSHED)  (((NPUSHED) + 3) & ~3)

/* Offset of first parameter from the argument pointer register value.  */

#define FIRST_PARM_OFFSET(FNDECL)  0

/* Value is the number of byte of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0 

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

#define FUNCTION_VALUE(VALTYPE, FUNC) \
   gen_rtx_REG (TYPE_MODE (VALTYPE), G4_REG)

/* 1 if N is a possible register number for a function value
   as seen by the caller.  */

#define FUNCTION_VALUE_REGNO_P(N)  \
  ((N) == G4_REG)

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(N) 0

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE) \
  gen_rtx_REG (MODE, G4_REG)

/* Define this macro to be a nonzero value if the location where a
   function argument is passed depends on whether or not it is a
   named argument.  */

#define STRICT_ARGUMENT_NAMING 1

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   For picoJava this is a struct which remembers the number of
   arguments named, the total number of words passed and an adjustment
   factor to use if accessing a double word argument with a single
   word memop.  See the comments at the head pj.c for more information */

#define ARGS_IN_REGS 32

struct b16_args
{
  int named_words;
  int total_words;
  int arg_count;
  int arg_adjust[ARGS_IN_REGS];
};

#define CUMULATIVE_ARGS  struct b16_args

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.
 */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT) \
  (CUM).named_words = 0;                                     \
  (CUM).total_words = 0;                                     \
  (CUM).arg_count = 0;

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.

   b16 only ever sends scalars as arguments.  Aggregates are sent
   by reference.  */

#define B16_ARG_WORDS(MODE)  \
   ((GET_MODE_SIZE (MODE) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)                    \
{                                                                       \
  (CUM).total_words += B16_ARG_WORDS (MODE);                             \
  if (NAMED)                                                            \
    (CUM).named_words += B16_ARG_WORDS (MODE);                           \
  (CUM).arg_count++;                                                    \
}

/* Define where to put the arguments to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).

   For picoJava scalar arguments are normally in registers.  */


#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)                            \
    ( ((CUM).total_words + B16_ARG_WORDS (MODE) < ARGS_IN_REGS)          \
       ? gen_rtx (REG, MODE, O0_REG + (CUM).total_words)                \
       : NULL_RTX)


/* A C expression that indicates when an argument must be passed by
   reference.  If nonzero for an argument, a copy of that argument is
   made in memory and a pointer to the argument is passed instead of
   the argument itself.  The pointer is passed in whatever way is
   appropriate for passing a pointer to that type.  */

/* All aggregates and arguments larger than 8 bytes are passed this way.  */

#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) \
 (TYPE && (AGGREGATE_TYPE_P (TYPE) || int_size_in_bytes (TYPE) > 8))

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 0

/* Trampoline support.  */

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */

/* On the b16, a trampoline contains three words:
   # # goto <static> <function>
*/

#define TRAMPOLINE_SIZE 6

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT) \
  b16_initialize_trampoline ((TRAMP), (FNADDR), (CXT))

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)                        \
 fprintf (FILE, "\t%d # >a 1 # a@ + a!*\n", (LABELNO));


/* Addressing modes, and classification of registers for them.  */

#define HAVE_POST_INCREMENT  0
#define HAVE_PRE_INCREMENT   0
#define HAVE_POST_DECREMENT  0
#define HAVE_PRE_DECREMENT   0

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

/* Any register is OK for a base or an index.  As is something that has
   been spilled to memory.  */

#define REGNO_OK_FOR_BASE_P(REGNO) 1
#define REGNO_OK_FOR_INDEX_P(REGNO) 1

/* Maximum number of registers that can appear in a valid memory
   address.  

   Arbitarily limited to 20.  */

#define MAX_REGS_PER_ADDRESS 20

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)                                   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF      \
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST)

/* Nonzero if the constant value X is a legitimate general operand.  */

#define LEGITIMATE_CONSTANT_P(X) \
  (GET_CODE (X) != CONST_DOUBLE)

/* Letters in the range `Q' through `U' in a register constraint string
   may be defined in a machine-dependent fashion to stand for arbitrary
   operand types.

   For picoJava, `S' handles a source operand. */

#define EXTRA_CONSTRAINT(OP, C) 0

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx and
   check its validity for a certain class.  */

#define REG_OK_FOR_BASE_P(X) 1
#define REG_OK_FOR_INDEX_P(x) 0


/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.  

   We may have arbitrarily complex addressing modes, but we get better
   cse of address expressions if we generate code with simple
   addressing modes and clean up redundant register operations later
   in the machine dependent reorg pass.  */

#define SRC_REG_P(X) (REG_P(X))

#define SIMPLE_ADDRESS(X) \
 (SRC_REG_P(X) || CONSTANT_ADDRESS_P(X))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)                          \
   if (SIMPLE_ADDRESS(X)) goto LABEL;                                     \
   if ((GET_CODE (X) == POST_INC                                          \
       || GET_CODE (X) == PRE_INC                                         \
       || GET_CODE (X) == POST_DEC                                        \
       || GET_CODE (X) == PRE_DEC) && SRC_REG_P(XEXP (X, 0))) goto LABEL; \

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)                        \
{                                                                       \
  if (GET_CODE(ADDR) == PRE_DEC || GET_CODE(ADDR) == POST_INC           \
      || GET_CODE(ADDR) == PRE_INC || GET_CODE(ADDR) == POST_DEC)       \
    goto LABEL;                                                         \
}

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.  */

#define CASE_VECTOR_PC_RELATIVE 1

/* 'char' is signed by default.  */
#define DEFAULT_SIGNED_CHAR  1

/* The type of size_t unsigned int.  */
#define SIZE_TYPE "unsigned int"

/* Don't cse the address of the function being compiled.  */

#define NO_RECURSIVE_FUNCTION_CSE (!optimize_size)

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */

#define MOVE_MAX 2

/* Max number of bytes we want move_by_pieces to be able to copy
   efficiently.  */

#define MOVE_MAX_PIECES 2

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
/*#define WORD_REGISTER_OPERATIONS*/

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */

#define LOAD_EXTEND_OP(MODE) SIGN_EXTEND

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* Define if loading short immediate values into registers sign extends.  */

/* #define SHORT_IMMEDIATES_SIGN_EXTEND */

/* Nonzero if access to memory by bytes is no faster than for words.  */
#define SLOW_BYTE_ACCESS 1

#define SHORT_TYPE_SIZE		16
#define INT_TYPE_SIZE           16
#define LONG_TYPE_SIZE          32

/* A C expression that is nonzero if on this machine the number of
   bits actually used for the count of a shift operation is equal to the
   number of bits needed to represent the size of the object being
   shifted.  */

#define SHIFT_COUNT_TRUNCATED 1

/* All integers have the same format so truncation is easy.  */

#define TRULY_NOOP_TRUNCATION(OUTPREC,INPREC)  1

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */

#define NO_FUNCTION_CSE 1

/* Chars and shorts should be passed as ints.  */

#define PROMOTE_PROTOTYPES 1

/* The machine modes of pointers and functions.  */

#define Pmode  SImode
#define FUNCTION_MODE  Pmode


/* A part of a C `switch' statement that describes the relative costs
   of constant RTL expressions.  It must contain `case' labels for
   expression codes `const_int', `const', `symbol_ref', `label_ref'
   and `const_double'.  Each case must ultimately reach a `return'
   statement to return the relative cost of the use of that kind of
   constant value in an expression.  The cost may depend on the
   precise value of the constant, which is available for examination
   in X, and the rtx code of the expression in which it is contained,
   found in OUTER_CODE.
  
   CODE is the expression code--redundant, since it can be obtained
   with `GET_CODE (X)'.  */

/* Compute extra cost of moving data between one register class and
   another.  */

/* #define REGISTER_MOVE_COST(MODE, SRC_CLASS, DST_CLASS) */


/* Assembler output control.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at
   the end of the line.  */
#define ASM_COMMENT_START "\\ "

/* The text to go at the start of the assembler file.  */

#define ASM_APP_ON              ""
#define ASM_APP_OFF             ""
#define FILE_ASM_OP             "\\ file\n"

#define SET_ASM_OP              "\\ .set\t"

/* How to change between sections.  */

#define TEXT_SECTION_ASM_OP             "\\ .text"
#define DATA_SECTION_ASM_OP             "\\ .data"

/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as
   uninitialized G data.  If not defined, and neither
   `ASM_OUTPUT_BSS' nor `ASM_OUTPUT_ALIGNED_BSS' are defined,
   uninitialized global data will be output in the data section if
   `-fno-common' is passed, otherwise `ASM_OUTPUT_COMMON' will be
   used.  */

#define BSS_SECTION_ASM_OP      "\t.section\t.bss"

/* Like `ASM_OUTPUT_BSS' except takes the required alignment as a
   separate, explicit argument.  If you define this macro, it is used
   in place of `ASM_OUTPUT_BSS', and gives you more flexibility in
   handling the required alignment of the variable.  The alignment is
   specified as the number of bits.

   Try to use function `asm_output_aligned_bss' defined in file
   `varasm.c' when defining this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)


/* Define this so that jump tables go in same section as the current function,
   which could be text or it could be a user defined section.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* The assembler's names for the registers.  */

#define REGISTER_NAMES                                          \
{                                                               \
   "tor", "sp",  "r2",  "r3",  "r4",  "r5",  "r6", "r7",        \
   "r8", "r9",  "r10", "r11", "r12", "r13", "r14","r15",        \
   "r16","r17", "r18", "r19", "r20", "r21", "r22","r23",        \
   "r24","r25", "r26", "r27", "r28", "r29", "r30","r31",        \
                                                                \
   "r32", "r33", "r34", "r35", "r36", "r37", "r38", "r39",      \
   "r40", "r41", "r42", "r43", "r44", "r45", "r46", "r47",      \
   "r48", "r49", "r50", "r51", "r52", "r53", "r54", "r55",      \
   "r56", "r57", "r58", "r59", "r60", "r61", "r62", "r63",      \
   "r64", "r65", "r66", "r67", "r68", "r69", "r70", "r71",      \
   "r72", "r73", "r74", "r75", "r76", "r77", "r78", "r79",      \
   "r80", "r81", "r82", "r83", "r84", "r85", "r86", "r87",      \
   "r88", "r89", "r90", "r91", "r92", "r93", "r94", "r95",      \
   "r96", "r97", "r98", "r99", "r100", "r101", "r102", "r103",  \
   "r104", "r105", "r106", "r107", "r108", "r109", "r110", "r111",     \
   "r112", "r113", "r114", "r115", "r116", "r117", "r118", "r119",     \
   "r120", "r121", "r122", "r123", "r124", "r125", "r126", "r127" }

/* Output a label definition.  */

#define ASM_OUTPUT_LABEL(FILE,NAME) \
  do { assemble_name ((FILE), (NAME)); fputs (":\n", (FILE)); } while (0)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)      	\
  if ((LOG) != 0)                       	\
    fprintf ((FILE), "\t.align %d\n", (LOG))

/* After an opcode has been printed, there's nothing on the line any
   more.  */

#define ASM_OUTPUT_OPCODE(STREAM, P)

/* The prefix to add to user-visible assembler symbols.  */

#define USER_LABEL_PREFIX ""

/* The prefix to add to an internally generated label.  */

#define LOCAL_LABEL_PREFIX "."

/* Make an internal label into a string.  */
#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM) \
  sprintf ((STRING), "*%s%s%ld", LOCAL_LABEL_PREFIX, (PREFIX), (long)(NUM))

/* Output an internal label definition.  */

/* Construct a private name.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTVAR,NAME,NUMBER)     \
  ((OUTVAR) = (char *) alloca (strlen (NAME) + 10),     \
   sprintf ((OUTVAR), "%s.%d", (NAME), (NUMBER)))

/* Output a relative address table.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM,BODY,VALUE,REL)                 \
      asm_fprintf ((STREAM), "\t.long\t.L%d-.L%di\n", (VALUE),(REL));

#define ADDR_VEC_ALIGN(VEC) 0

/* Output various types of constants.  */

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#undef  ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE) \
  fprintf ((FILE), "\t.space %d\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)    \
( fputs ("\t.comm ", (FILE)),                   	\
  assemble_name ((FILE), (NAME)),               	\
  fprintf ((FILE), ",%d\n", (SIZE)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)     \
( fputs ("\t.lcomm ", (FILE)),                          \
  assemble_name ((FILE), (NAME)),                       \
  fprintf ((FILE), ",%d\n", (SIZE)))

/* We don't want the default switch handling.  */
#undef ASM_OUTPUT_BEFORE_CASE_LABEL
#undef ASM_OUTPUT_CASE_LABEL

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or star or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

#define PRINT_OPERAND(STREAM, X, CODE)  b16_print_operand ((STREAM), (X), (CODE))

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(STREAM,X)  output_addr_const (STREAM, X)

/* Punctuation valid for print_operand.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) ((CODE) == '*')

#define GLOBAL_ASM_OP "\t.global "


/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases, 
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.

   Since picoJava doesn't have unsigned compares, prefer signed
   arithmetic.  */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)             \
 if (GET_MODE_CLASS (MODE) == MODE_INT                  \
     && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)          \
     {                                                  \
       (MODE) = SImode;                                 \
       (UNSIGNEDP) = 0;                                 \
      }

/* Defining PROMOTE_FUNCTION_ARGS eliminates some unnecessary zero/sign
   extensions applied to char/short functions arguments.  Defining
   PROMOTE_FUNCTION_RETURN does the same for function returns.  */
#define PROMOTE_FUNCTION_ARGS


/* We can debug without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP 1

#define DBX_REGISTER_NUMBER(REG) (REG)

#define DONT_USE_BUILTIN_SETJMP

/* We prefer to use dwarf2. */
#undef  PREFERRED_DEBUGGING_TYPE 
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
#define DWARF2_UNWIND_INFO 1


/* varargs and stdarg builtins.  */

#define EXPAND_BUILTIN_VA_START(valist, nextarg)                      \
   std_expand_builtin_va_start(valist, nextarg)

#define EXPAND_BUILTIN_VA_ARG(valist, type)                             \
   std_expand_builtin_va_arg(valist, type)

#define EXPAND_BUILTIN_NEXT_ARG(OFFSET)                                 \
   gen_rtx_MEM (Pmode,                                                  \
                plus_constant (gen_rtx_REG (QImode, VARS_REG),          \
                               (INTVAL (offset) + 1) * -2));     

/* Before the prologue, RA is on the return stack.  */
/* !!! FIXME !!! this is just a dummy */
#define INCOMING_RETURN_ADDR_RTX  \
  gen_rtx_REG (VOIDmode, TOR_REGNUM)

/* Define the codes that are matched by predicates in b16.c.  */
#define PREDICATE_CODES 						 \
  {"b16_dest_operand",                 {SUBREG, REG, MEM}},               \
  {"b16_signed_comparison_operator",   {EQ, NE, LE, LT, GE, GT}},         \
  {"b16_unsigned_comparison_operator", {LEU, LTU, GEU, GTU}},             \
  {"b16_source_operand",               {CONST_INT, CONST_DOUBLE, CONST,   \
                                        SYMBOL_REF, LABEL_REF, SUBREG,    \
                                        REG, MEM}},

/* Generate calls to memcpy, memcmp and memset.  */
#define TARGET_MEM_FUNCTIONS
