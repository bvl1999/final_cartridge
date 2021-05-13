; ----------------------------------------------------------------
; I/O Area ROM
; ----------------------------------------------------------------
; This is a max. 512 byte section that lives at $1E00-$1FFF of
; bank 0 of the ROM, and will also be mapped into the I/O extension
; area at $DE00-$DFFF, so it's always visible.
; It mostly contains wrappers around BASIC, KERNAL or cartridge
; functions that switch the ROM config in addition.
.feature c_comments

.include "kernal.i"

; from printer
.import new_clrch
.import new_clall
.import new_getin
.import new_bsin
.import new_bsout
.import new_ckin
.import new_ckout
.import new_open
.import new_close
.import _nmi_alt_handler

; from basic
.import reset_warmstart
.import new_tokenize
.import new_execute
.import new_expression
.import new_detokenize
.import new_mainloop

; from editor
.import kbd_handler

; from wrapper
.import disable_rom_then_warm_start

; from speeder
.import new_save
.import new_load

; from desktop_helper
.import load_and_run_program

.segment        "romio1"
        .byte "ACIA"

LDE00:  .byte   $40

.global _jmp_bank
_jmp_bank:
        sta     $DFFF
        rts

.global _enable_rom
_enable_rom:
        php
        pha
        lda     #$40 ; bank 0
LDE08:  sta     $DFFF
        pla
        plp
        rts

.global _disable_rom_set_01
_disable_rom_set_01:
        sty     $01
.global _disable_rom
_disable_rom:
        php
        pha
        lda     #$70 ; no ROM at $8000; BASIC at $A000
        bne     LDE08

.global _basic_warm_start
_basic_warm_start:
        jsr     _disable_rom
        jmp     $E37B ; BASIC warm start (NMI)

enable_all_roms:  
        ora     #$07
        sta     $01
        bne     _enable_rom

.global _new_load
_new_load:
        tay
        lda     $01
        pha
        jsr     enable_all_roms
        jsr     new_load
LDE2B:  tax
        pla
        sta     $01
        txa
        ldx     $AE
        jmp     _disable_rom

.global _new_save
_new_save:
        lda     $01
        pha
        jsr     enable_all_roms
        jsr     new_save
        jmp     LDE2B

.global _new_mainloop
_new_mainloop:
        lda     $01
        jsr     enable_all_roms
        jmp     new_mainloop

.global _new_detokenize
_new_detokenize:
        jsr     _enable_rom
        jmp     new_detokenize

.global _new_expression
_new_expression:
        jsr     _enable_rom
        jmp     new_expression

.global _kbd_handler
_kbd_handler:
        lda     FC3_KBFLAG
        beq     LDE5D
        jmp     $EB42 ; LDA #$7F : STA $DC00 : RTS

LDE5D:  lda     $A000
        jmp     LDF80

.global _load_ac_indy
_load_ac_indy:
        sta     $01
        lda     ($AC),y
        inc     $01
        inc     $01
        rts

.global _load_FNADR_indy
_load_FNADR_indy:
        dec     $01
        lda     (FNADR),y
        inc     $01
        rts

.global _new_execute
_new_execute:
        jsr     _CHRGET
        jsr     new_execute
        jsr     _disable_rom
        jmp     $A7AE ; CLEAR

.global _execute_statement
_execute_statement:
        jsr     _disable_rom
        jmp     $A7EF ; execute BASIC statement

.global _add_A_to_FAC
_add_A_to_FAC:
        jsr     _disable_rom
        jsr     $BD7E ; add A to FAC
        jmp     _enable_rom

.global _expression_cont
_expression_cont:
        jsr     _disable_rom
        jmp     $AE8D ; get element in expression

.global _get_int
_get_int:
        jsr     _disable_rom
        jsr     $AD8A ; FRMNUM eval expression, make sure it's numeric
        jsr     $B7F7 ; GETADR convert FAC into 16 bit int
        jmp     _enable_rom

.global _new_warmstart
_new_warmstart:
        jsr     _enable_rom
        jsr     reset_warmstart
        jmp     disable_rom_then_warm_start

.global _evaluate_modifier
_evaluate_modifier:
        jsr     _disable_rom
        jmp     $EB48 ; evaluate SHIFT/CTRL/C=

.global _get_line_number
_get_line_number:
        jsr     _disable_rom
        jsr     $A96B ; get line number
        jmp     _enable_rom

.global _basic_bsout
_basic_bsout:
        jsr     _disable_rom
        jsr     $AB47 ; print character
        jmp     _enable_rom

.global _check_for_stop
_check_for_stop:
        jsr     _disable_rom
        jsr     $A82C ; check for STOP
        jmp     _enable_rom

.global _relink
_relink:
        jsr     _disable_rom
        jsr     $A533 ; rebuild BASIC line chaining
        beq     LDEE1 ; branch always?

.global _get_filename
_get_filename:
        jsr     _disable_rom
        jsr     $E257 ; get string from BASIC line, set filename
LDEE1:  jmp     _enable_rom

.global _int_to_ascii
_int_to_ascii:
        jsr     _disable_rom
        jsr     $BC49 ; FLOAT UNSIGNED VALUE IN FAC+1,2
        jsr     $BDDD ; convert FAC to ASCII
        jmp     _enable_rom

.global _search_for_line
_search_for_line:
        jsr     _disable_rom
        jsr     $A613 ; search for BASIC line
        jsr     _enable_rom
        rts

.global _ay_to_float
_ay_to_float:
        jsr     _disable_rom
        jsr     $B395 ; convert A/Y to float
        jmp     LDEFF

.segment        "romio2"
        .byte "REU REU REU REU "
        .byte "REU REU REU UCI "

.global _int_to_fac
_int_to_fac:
        jsr     _disable_rom
        jsr     $BBA6 ; convert $22/$23 to FAC
LDEFF:  iny
        jsr     $BDD7 ; print FAC
        jmp     _enable_rom


.global _print_ax_int
_print_ax_int:
        jsr     _disable_rom
        jsr     $BDCD ; LINPRT print A/X as integer
        jmp     _enable_rom

.global _CHRGET
_CHRGET:
        jsr     _disable_rom
        jsr     CHRGET
LDF21:  jsr     _enable_rom
        rts

.global _CHRGOT
_CHRGOT:
        jsr     _disable_rom
        jsr     CHRGOT
        jmp     LDF21

.global _lda_5a_indy
_lda_5a_indy:
        jsr     _disable_rom
        lda     ($5A),y
        jmp     _enable_rom

.global _lda_5f_indy
_lda_5f_indy:
        jsr     _disable_rom
        lda     ($5F),y
        jmp     _enable_rom

.global _lda_ae_indx
_lda_ae_indx:
        jsr     _disable_rom
        lda     ($AE,x)
        jmp     _enable_rom

.global _lda_TXTPTR_indy
_lda_TXTPTR_indy:
        jsr     _disable_rom
        lda     (TXTPTR),y
        jmp     _enable_rom

.global _lda_TXTPTR_indx
_lda_TXTPTR_indx: ; DF50
        jsr     _disable_rom
        lda     (TXTPTR,x)
        jmp     _enable_rom

.global _lda_22_indy
_lda_22_indy:
        jsr     _disable_rom
        lda     ($22),y
        jmp     _enable_rom

.global _lda_8b_indy
_lda_8b_indy:
        jsr     _disable_rom
        lda     ($8B),y
        jmp     _enable_rom

_detokenize:
        jsr     _disable_rom
        jmp     $A724 ; detokenize

.global _list
_list:
        jsr     _disable_rom
        jmp     $A6F3 ; part of LIST

LDF80:  cmp     #$94 ; contents of $A000 in BASIC ROM
        bne     LDF8A ; BASIC ROM not visible
        jsr     _enable_rom
        jmp     kbd_handler

LDF8A:  jmp     $EB48 ; default kdb vector

.global _new_tokenize
_new_tokenize:
        jsr     _enable_rom
        jsr     new_tokenize
        jmp     _disable_rom

; calls into banks 0+1
.global _new_ckout
_new_ckout:
        jsr     _enable_rom
        jmp     new_ckout

.global _new_bsout
_new_bsout:
        jsr     _enable_rom
        jmp     new_bsout

.global _new_clall
_new_clall:
        jsr     _enable_rom
        jmp     new_clall

.global _new_clrch
_new_clrch:
        jsr     _enable_rom
        jmp     new_clrch

.global _new_open
_new_open:
        jsr     _enable_rom
        jmp     new_open

.global _new_close
_new_close:
        jsr     _enable_rom
        jmp     new_close

.global _new_ckin
_new_ckin:
        jsr     _enable_rom
        jmp     new_ckin

.global _new_bsin
_new_bsin:
        lda DEVFROM
        cmp UCI_DEVICE
        beq :+
        jmp (CHRIN_ORIG)
:
        jsr _enable_rom
        jmp new_bsin

.global _new_getin
_new_getin:
        jsr     _enable_rom
        jmp     new_getin

_new_nmi:
        jsr     _enable_rom
        jsr     _nmi_alt_handler
        lda     #$70
        sta     $DFFF
        jmp     $fe72

/*
.global LDFE0
LDFE0:
        sei
        lda     #$42 ; bank 2 (Desktop, Freezer/Print)
        sta     $DFFF
.global _bar_irq
_bar_irq:
        lda     LDE00 ; $40 ???
        pha
        lda     $A000 ; ???
        pha
        lda     #$41 ; bank 1 (Notepad, BASIC (Menu Bar))
        sta     $DFFF
*/
.global _a_colon_asterisk
_a_colon_asterisk:
        .byte   ':','*'
.global _a_colon_asterisk_end
_a_colon_asterisk_end:

; ----------------------------------------------------------------
; I/O Area ROM End
; ----------------------------------------------------------------
