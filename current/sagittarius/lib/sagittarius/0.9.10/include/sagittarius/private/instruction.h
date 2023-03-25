/* Generated automatically from boot/instructions.scm */
/* DO NOT EDIT */

#ifndef SAGITTARIUS_INSTRUCATIONS_H
#define SAGITTARIUS_INSTRUCATIONS_H

#include "sagittariusdefs.h"
#define INSN_MASK 0xFF
#define INSN_VALUE1_MASK  0xFFF
#define INSN_VALUE2_MASK  (uintptr_t)-1
#define INSN_VALUE1_SHIFT 8
#define INSN_VALUE2_SHIFT 20
#if defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
/* what a stupid macro definition on windows.h */
#undef CONST
#endif

typedef enum {
  NOP = 0x00,
  HALT = 0x01,
  UNDEF = 0x02,
  CONST = 0x03,
  CONSTI = 0x04,
  LREF = 0x05,
  LSET = 0x06,
  FREF = 0x07,
  FSET = 0x08,
  GREF = 0x09,
  GSET = 0x0a,
  PUSH = 0x0b,
  BOX = 0x0c,
  UNBOX = 0x0d,
  ADD = 0x0e,
  ADDI = 0x0f,
  SUB = 0x10,
  SUBI = 0x11,
  MUL = 0x12,
  MULI = 0x13,
  DIV = 0x14,
  DIVI = 0x15,
  NEG = 0x16,
  TEST = 0x17,
  JUMP = 0x18,
  SHIFTJ = 0x19,
  BNNUME = 0x1a,
  BNLT = 0x1b,
  BNLE = 0x1c,
  BNGT = 0x1d,
  BNGE = 0x1e,
  BNEQ = 0x1f,
  BNEQV = 0x20,
  BNNULL = 0x21,
  NOT = 0x22,
  NUM_EQ = 0x23,
  NUM_LT = 0x24,
  NUM_LE = 0x25,
  NUM_GT = 0x26,
  NUM_GE = 0x27,
  RECEIVE = 0x28,
  CLOSURE = 0x29,
  APPLY = 0x2a,
  CALL = 0x2b,
  LOCAL_CALL = 0x2c,
  TAIL_CALL = 0x2d,
  LOCAL_TAIL_CALL = 0x2e,
  RET = 0x2f,
  FRAME = 0x30,
  INST_STACK = 0x31,
  LEAVE = 0x32,
  DEFINE = 0x33,
  LIBRARY = 0x34,
  CAR = 0x35,
  CDR = 0x36,
  CONS = 0x37,
  LIST = 0x38,
  APPEND = 0x39,
  VALUES = 0x3a,
  EQ = 0x3b,
  EQV = 0x3c,
  NULLP = 0x3d,
  PAIRP = 0x3e,
  SYMBOLP = 0x3f,
  VECTOR = 0x40,
  VECTORP = 0x41,
  VEC_LEN = 0x42,
  VEC_REF = 0x43,
  VEC_SET = 0x44,
  LREF_PUSH = 0x45,
  FREF_PUSH = 0x46,
  GREF_PUSH = 0x47,
  CONST_PUSH = 0x48,
  CONSTI_PUSH = 0x49,
  GREF_CALL = 0x4a,
  GREF_TAIL_CALL = 0x4b,
  SET_CAR = 0x4c,
  SET_CDR = 0x4d,
  CAAR = 0x4e,
  CADR = 0x4f,
  CDAR = 0x50,
  CDDR = 0x51,
  CAR_PUSH = 0x52,
  CDR_PUSH = 0x53,
  CONS_PUSH = 0x54,
  LREF_CAR = 0x55,
  LREF_CDR = 0x56,
  FREF_CAR = 0x57,
  FREF_CDR = 0x58,
  GREF_CAR = 0x59,
  GREF_CDR = 0x5a,
  LREF_CAR_PUSH = 0x5b,
  LREF_CDR_PUSH = 0x5c,
  FREF_CAR_PUSH = 0x5d,
  FREF_CDR_PUSH = 0x5e,
  GREF_CAR_PUSH = 0x5f,
  GREF_CDR_PUSH = 0x60,
  CONST_RET = 0x61,
  APPLY_VALUES = 0x62,
  RESV_STACK = 0x63,
} Instruction;
#define INSTRUCTION_COUNT 100 /** number of instructions */
typedef struct InsnInfoRec InsnInfo;
struct InsnInfoRec
{
  const char *name;
  int         number;
  int         instValues;
  int         argc;
  int         hasSrc;
  int         label;
};
#define INSN(o)            ((o) & INSN_MASK)
#define INSN_VALUE1(insn) (((int)(insn)) >> INSN_VALUE1_SHIFT)
#define INSN_VALUE2(insn) (((int)(insn)) >> INSN_VALUE2_SHIFT)
#define INSN_VAL1(v, insn) ((v) = INSN_VALUE1(insn))
#define INSN_VAL2(v1, v2, insn)	\
  do {				\
    (v1) = (INSN_VALUE1(insn) & INSN_VALUE1_MASK);	\
    (v2) = (INSN_VALUE2(insn) & INSN_VALUE2_MASK);	\
  } while (0)
#define MERGE_INSN_VALUE1(insn, value)      \
  ((insn) | ((value) << INSN_VALUE1_SHIFT))
#define MERGE_INSN_VALUE2(insn, val1, val2) \
  ((insn) | ((val1) << INSN_VALUE1_SHIFT) | ((val2) << INSN_VALUE2_SHIFT))
SG_CDECL_BEGIN
SG_EXTERN InsnInfo* Sg_LookupInsnName(Instruction insn);
SG_CDECL_END
#endif

