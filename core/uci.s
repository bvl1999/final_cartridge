;----------------------------------------------------
; Routines for loading / saving / fileio through UCI
;
; Programmed by Gideon Zweijtzer
;
; Copyright (c) 2020 - Gideon Zweijtzer
;
;----------------------------------------------------
.include "kernal.i"
.include "persistent.i"
.segment "ultimate"
.feature labels_without_colons, c_comments

.global new_open2
.global new_close2
.global new_ckin2
.global new_bsin2
.global new_getin2
.global ulti_clrchn
.global ulti_ckout
.global ulti_chrout
.global ckin_known_fasa
.global new_load
.global new_save

.import hide
.import unhide


;
        CMD_IF_CONTROL = $DF1C
        CMD_IF_COMMAND = $DF1D
        CMD_IF_RESULT  = $DF1E
        CMD_IF_STATUS  = $DF1F

        UCI_IDENTIFIER = $C9
        CMD_PUSH_CMD   = $01
        CMD_NEXT_DATA  = $02
        CMD_ABORT      = $04
        CMD_ERROR      = $08

        CMD_STATE_BITS      = $30
        CMD_STATE_DATA      = $20
        CMD_STATE_IDLE      = $00
        CMD_STATE_BUSY      = $10
        CMD_STATE_LAST_DATA = $20
        CMD_STATE_MORE_DATA = $30

        UCI_TARGET     = $05
        UCI_CMD_LOADSU = $10
        UCI_CMD_LOADEX = $11
        UCI_CMD_SAVE   = $12
        UCI_CMD_OPEN   = $13
        UCI_CMD_CLOSE  = $14
        UCI_CMD_CHKIN  = $15
        UCI_CMD_CHKOUT = $16

/*
        OPEN_VECTOR   = $031A
        CLOSE_VECTOR  = $031C
        CHKIN_VECTOR  = $031E
        CHKOUT_VECTOR = $0320
        CLRCHN_VECTOR = $0322
        CHRIN_VECTOR  = $0324
        CHROUT_VECTOR = $0326
        GETIN_VECTOR  = $032A
        LOAD_VECTOR   = $0330
        SAVE_VECTOR   = $0332
*/

        STATUS     = $90
        VERIFYFLAG = $93
;        DEVFROM    = $99
        DEVTO      = $9A
        LOADPNTR   = $AE
        SECADDR    = $B9
        DEVNUM     = $BA
        NAMEPTR    = $BB
        NAMELEN    = $B7
        LOADADDR   = $C3
        SAVEADDR   = $C1
        SAVEEND    = $AE

;        FILE_NOT_FOUND_ERROR    = $F704
        FILE_LOOKUP_A           = $F314
        FILE_LOOKUP_X           = $F30F ; Might be different for JD!
        GET_FILE_PARAMS         = $F31F ; Might be different for JD!
        CHKIN_CONTINUED         = $F219 ; Might be different for JD!


.macro fc3exit addr
            lda #>(addr-1)
            pha
            lda #<(addr-1)
            pha
            jmp _disable_rom
.endmacro

/*
ulti_restor
            lda #9
            sta UCI_DEVICE
            jsr uci_clear_error
            jmp restor
*/

; $FFD5 
; LOAD. Load or verify file. (Must call SETLFS and SETNAM beforehand.)
; Input: A: 0 = Load, 1-255 = Verify; X/Y = Load address (if secondary address = 0).
; Output: Carry: 0 = No errors, 1 = Error; A = KERNAL error code (if Carry = 1); X/Y = Address of last byte loaded/verified (if Carry = 0).
; Used registers: A, X, Y.
; Real address: $F49E.
; Vectors through $0330, after storing X in $C3 and Y in $C4 (LOADADDR)


.global ulti_load
ulti_load:
            lda #$00
            sta STATUS
            ldy NAMELEN
            bne ld1
            lda #8 ; missing filename
            sec
            rts

ld1         lda CMD_IF_COMMAND
            cmp #UCI_IDENTIFIER
            beq ld2
            lda #5 ; device not present error
            sec
            rts

ld2         php
            sei
            jsr print_searching
            ldx #UCI_CMD_LOADSU
            jsr uci_setup_cmd
            ldy #LOADADDR
            jsr uci_setup_range
            jsr uci_filename
            jsr uci_execute
            lda CMD_IF_STATUS
            beq ld3 ; all OK when zero
            jsr uci_ack ; restores A
            lda #4  ; File not found error
            plp
            sec
            rts

ld3         jsr print_loading

            jsr uci_ack
            ldx #UCI_CMD_LOADEX
            jsr uci_setup_cmd
            jsr uci_execute
            lda CMD_IF_STATUS
            bmi _verify_err

            lda CMD_IF_STATUS
            sta LOADPNTR
            lda CMD_IF_STATUS
            sta LOADPNTR+1
            ;jsr show_end_addr

            jsr uci_ack ; restores A
            ldx LOADPNTR
            ldy LOADPNTR+1
            plp
            clc
            rts

_verify_err jsr uci_ack ; restores A
            lda #$10
            ora STATUS
            sta STATUS
            plp
            clc
            rts


; $FFD8   
; SAVE. Save file. (Must call SETLFS and SETNAM beforehand.)
; Input: A = Address of zero page register holding start address of memory area to save; X/Y = End address of memory area plus 1.
; Output: Carry: 0 = No errors, 1 = Error; A = KERNAL error code (if Carry = 1).
; Used registers: A, X, Y.
; Real address: $F5DD.
; Vector through $0332, after storing start address in $C1 and $C2 and end address in $AE and $AF

.global ulti_save
ulti_save:
            lda #$00
            sta STATUS
            ldy NAMELEN
            bne sv1
            lda #8 ; missing filename
            sec
            rts

sv1         lda CMD_IF_COMMAND
            cmp #UCI_IDENTIFIER
            beq sv2
            lda #5 ; device not present
            sec
            rts

sv2         php
            sei
            jsr print_saving
            ldx #UCI_CMD_SAVE
            jsr uci_setup_cmd
            ldy #SAVEADDR
            jsr uci_setup_range
            jsr uci_filename
            jsr uci_execute_from_ram
            lda CMD_IF_STATUS
            beq sv3 ; all OK when zero

            jsr uci_ack
            plp
            sec
            lda #7 ; not output file :)
            rts

sv3         jsr uci_ack
            plp
            clc
            rts

.segment "ultimate2"

; ----------------------------------------------------------------
new_open2:
; $FFC0   
; OPEN. Open file. (Must call SETLFS and SETNAM beforehand.)
; Input: –
; Output: –
; Used registers: A, X, Y.
; Real address: ($031A), $F34A.
; 
            lookup = $F30F
            ldtnd  = $98
            la     = $B8
            sa     = $B9
            fa     = $BA
            lat    = $0259
            sat    = $026D
            fat    = $0263

            ldx la          ;check file #
            beq error6      ;is the keyboard -> not input file

            lda DEVNUM
            cmp UCI_DEVICE
            beq myopen

op1         ; The OPEN_ORIG function may use CLRCHN and BSOUT. Secondly, since it needs to access the file name, the rom should also be turned off.
            ; For this reason, we simply exit the FC3 ROM here and call the original open function from outside of the ROM.
            ; NOTE: THIS IS ONLY ALLOWED IF THE OPEN FUNCTION IS NOT CALLED FROM WITHIN THE ROM ITSELF. The RTS at the end of the
            ; OPEN_ORIG function brings us back to the orignal called with the ROM off.. so it cannot be part of the ROM itself.
            ;
            ; Note, that if open were to be called from the ROM, a copy of the kernal 'OPEN' routine must be implemented, with all the diversity
            ; of paths; including RS232 support and all.
            ;
            ; BAD assumption:
            ; Because we can only be called from outside of the rom, we MUST have entered here through persistent jump. Thus the vectors are for
            ; hidden ROM, and must stay that way. No need to set new vectors.
            ; in fact we get called from printer.s, so we need to remove the address jsr left on the stack

            pla
            pla

            lda OPEN_ORIG+1
            pha
            lda OPEN_ORIG
            sec
            sbc #1
            pha
            jmp _disable_rom

myopen      lda CMD_IF_COMMAND
            cmp #UCI_IDENTIFIER
            bne error5

            ; The following is an optimized copy of the kernal code at F34A, because
            ; it is common code, which should always be executed, but it
            ; cannot be vectored.

            jsr lookup      ;see if in table
            beq error2      ;found... -> file open
            ldx ldtnd       ;logical device table end
            cpx #10         ;maximum # of open files
            bcs error1      ;Too many files
            inc ldtnd       ;new file
            lda la
            sta lat,x       ;store logical file #
            lda sa
            ora #$60        ;make sa an serial command
            sta sa
            sta sat,x       ;store command #
            lda fa
            sta fat,x       ;store device #

            ldx #UCI_CMD_OPEN
            jsr uci_setup_cmd
            jsr uci_filename
            jsr uci_execute
            jsr uci_ack
            clc
            rts

error1      lda #1
            .byte $2c
error2      lda #2
            .byte $2c
error3      lda #3
            .byte $2c
error5      lda #5
            .byte $2c
error6      lda #6
            sec
            rts

; ----------------------------------------------------------------
new_close2:
; $FFC3
; CLOSE. Close file.
; Input: A = Logical number.
; Output: –
; Used registers: A, X, Y.
; Real address: ($031C), $F291.
;
        ; should always exit with RTS!
            pha
            jsr FILE_LOOKUP_A
            beq cl1
            ; not found, so close was not necessary
            pla
            clc
            rts

            ; x is now set to index and stack has original a
cl1         jsr GET_FILE_PARAMS

            lda DEVNUM
            cmp UCI_DEVICE
            beq myclose
cl2         pla ; restore stack for exit

            jmp (CLOSE_ORIG)   ; And do the lookup again. ;)
            ; Note that we are jumping to CLOSE_ORIG with ROM on.
            ; This is not a problem, as the RAM is not queried for the filename; neither does it jump to CLRCHN or BSOUT to print an error.
            ; We will return in the rom; as RTS brings us to new_close, which then jumps to _disable_rom.

myclose     
            lda CMD_IF_COMMAND
            cmp #UCI_IDENTIFIER
            bne cl2

            ldx #UCI_CMD_CLOSE
            jsr uci_setup_cmd
            jsr uci_execute
            jsr uci_ack
            pla ; restore stack
            lda DEVNUM
            jmp $f2f2 ; jx150+1, skip pla at the start
                      ; rts of this will return to new_close in persistent.s, which will call _disable_rom.
                      ; it seems this might error when a known fasa condition occured earlier.
;            jsr $f2f2
;            bcc :+
;            inc $d020
;:           rts

; ----------------------------------------------------------------
new_ckin2:
; $FFC6
; CHKIN. Define file as default input. (Must call OPEN beforehand.)
; Input: X = Logical number.
; Output: –
; Used registers: A, X.
; Real address: ($031E), $F20E.
;
        ; should always exit with RTS!
            jsr FILE_LOOKUP_X       ; as copied from stock kernal
            bne error3              ; as copied from stock kernal
_cki1       jsr GET_FILE_PARAMS     ; as copied from stock kernal

ckin_known_fasa:
            lda DEVNUM
            beq _ckin_ok
            cmp UCI_DEVICE
            beq _my_chkin
            cmp #4
            bcs _ckin_iec

            ; non-IEC, i.e. device 1, 2, or 3
            pla
            pla
            lda #>(CHKIN_CONTINUED-1)
            pha
            lda #<(CHKIN_CONTINUED-1)
            pha
            jmp _disable_rom
            ; Continue to the original Kernal code with ROM disabled. We do this because
            ; the original CKIN function may call upon CLRCHN and BSOUT, which are vectored.
            ; Consequence: This routine MAY NOT BE CALLED FROM INSIDE THE ROM!
            ; Fix: for all IEC devices, the following code has been copied from the kernal ROM,
            ; so that this function can be called from inside the ROM (for IEC and UCI only).

_ckin_iec   tax ; save A for later
            jsr TALK
            lda SA
            bpl :+
            jsr $EDCC   ; release ATN and wait for clock
            jmp :++
:
            jsr TKSA    ; send talk secondary
:
            txa
            bit ST
            bpl _ckin_ok
            lda #5      ; device not present
            sec
            rts


_my_chkin   sta DEVFROM
            lda DEVFROM
do_chkin    ldx #UCI_CMD_CHKIN
            jsr uci_setup_cmd
            jsr uci_execute
            clc
            rts
_ckin_ok    sta DEVFROM
            clc
            rts

; .segment "ultimate2"

; ----------------------------------------------------------------
new_bsin2:
; $FFCF
; CHRIN. Read byte from default input (for keyboard, read a line from the screen). (If not keyboard, must call OPEN and CHKIN beforehand.)
; Input: –
; Output: A = Byte read.
; Used registers: A, Y.
; Real address: ($0324), $F157.

        ; should always exit with RTS!
            lda DEVFROM
            cmp UCI_DEVICE
            beq my_chrin
            jmp (CHRIN_ORIG) ; runs with the ROM on.. which is valid. its RTS will bring us to new_bsin; which performs _disable_rom
;
my_chrin    ; is there any data available in the current buffer?
            lda CMD_IF_CONTROL
            bpl _no_data_avail

            ; read available data and store it on the stack
            lda CMD_IF_RESULT
            pha

            ; a byte was succesfully read. However, was this the last byte?
            lda CMD_IF_CONTROL
            bmi _ok   ; there is more in the current buffer

            ; end of current buffer. Is this the last buffer?
            and #CMD_STATE_BITS
            cmp #CMD_STATE_MORE_DATA
            beq _ok   ; there is a next buffer. So we are fine

            ; No next buffer available, so set EOI
            lda #$40
            ora STATUS
            sta STATUS

            ; pick up the byte we read and leave
_ok         pla
            clc
            rts ; done!!

_no_data_avail
            ; Current buffer is empty. Are we in the MORE data state?
            and #CMD_STATE_BITS
            cmp #CMD_STATE_MORE_DATA
            bne _read_error

            ; Get next block of data
            jsr uci_ack
            jsr uci_wait_busy
            jmp my_chrin

_read_error
            ; No data could be read. We return $0D, and set EOF + Read Error
            lda #$42
            ora STATUS
            sta STATUS
            lda #$0D
            clc
            rts

new_getin2:
; $FFE4
; GETIN. Read byte from default input. (If not keyboard, must call OPEN and CHKIN beforehand.)
; Input: –
; Output: A = Byte read.
; Used registers: A, X, Y.
; Real address: ($032A), $F13E.

getin       lda DEVFROM
            cmp UCI_DEVICE
            beq new_bsin2
            jmp (GETIN_ORIG) ; runs with the ROM on, which should be OK. It's RTS will bring us to _disable_rom


; $FFCC
; CLRCHN. Close default input/output files (for serial bus, send UNTALK and/or UNLISTEN); restore default input/output to keyboard/screen.
; Input: –
; Output: –
; Used registers: A, X.
; Real address: ($0322), $F333.
;
; This is called as a subroutine from FC3 code
ulti_clrchn:
            lda UCI_DEVICE
            cmp DEVTO
            beq _my_clrchn
            cmp DEVFROM
            beq _my_clrchn
            cmp DEVNUM
            beq _my_clrchn
            
_clr2       rts

_my_clrchn  lda CMD_IF_COMMAND
            cmp #UCI_IDENTIFIER
            bne _clr2
            jmp uci_abort ; will handle both cases of pending command and pending data state

ulti_ckout:
            lda #0
            sta UCI_OUTLEN
            sta STATUS

            lda CMD_IF_COMMAND
            cmp #UCI_IDENTIFIER
            beq :+

            lda #5
            sec
            ror STATUS
            rts

:           lda SECADDR
            and #$F0
            cmp #$F0
            beq _opn
            cmp #$E0
            beq _clse
            ldx #UCI_CMD_CHKOUT
            .byte $2c
_opn        ldx #UCI_CMD_OPEN
            .byte $2c
_clse       ldx #UCI_CMD_CLOSE
            jsr uci_abort
            jsr uci_setup_cmd    ; do not execute command, because we are waiting for data now
            lda #0
            clc
            rts

ulti_chrout:
            inc UCI_OUTLEN
            lda UCI_OUTLEN
            beq _breakup_out

_co1        pla
            sta CMD_IF_COMMAND ; Append the byte to write to the current command
            clc
            rts
_breakup_out
            txa
            pha
            jsr uci_execute    ; Execute the complete command, e.g. write the block of data
            jsr uci_ack
            jsr ulti_ckout     ; Send a new command to start transmission of the next block
            pla
            tax
            jmp _co1



;; UCI
uci_setup_cmd
            jsr uci_abort ; this will also abort an open command by executing it
            sec
            ror UCI_PENDING_CMD
            lda #UCI_TARGET
            sta CMD_IF_COMMAND
            stx CMD_IF_COMMAND
            lda SECADDR
            sta CMD_IF_COMMAND
            lda VERIFYFLAG
            sta CMD_IF_COMMAND
            rts

uci_setup_range
            lda $00,y
            sta CMD_IF_COMMAND
            lda $01,y
            sta CMD_IF_COMMAND
            lda SAVEEND
            sta CMD_IF_COMMAND
            lda SAVEEND+1
            sta CMD_IF_COMMAND
            rts

uci_filename
            php
            sei
            lda NAMELEN
            beq _fn2
            ldy #$00
_fn1        jsr _load_FNADR_indy
            sta CMD_IF_COMMAND
            iny
            cpy NAMELEN
            bne _fn1
_fn2        plp
            rts

uci_execute lda #CMD_PUSH_CMD
            sta CMD_IF_CONTROL
            lda #0
            sta UCI_PENDING_CMD
uci_wait_busy
_wb1        lda CMD_IF_CONTROL
            and #CMD_STATE_BITS
            cmp #CMD_STATE_BUSY
            beq _wb1
            ; we should now be in the data state, where we can also read the status
            rts

uci_ack     pha
            lda #CMD_NEXT_DATA
            sta CMD_IF_CONTROL
_ack1       lda CMD_IF_CONTROL
            and #CMD_NEXT_DATA
            bne _ack1
            pla
            rts

uci_abort   ; may be in command state
            bit UCI_PENDING_CMD
            bpl _abrt2 ; Bit 7 not set, no pending command
            jsr uci_execute
            jsr uci_ack
_abrt2      lda CMD_IF_CONTROL
            and #CMD_STATE_DATA
            beq _abrt1 ; NOT in Data state
            ; Perform Abort of current command
            lda #CMD_ABORT
            sta CMD_IF_CONTROL
            jsr uci_wait_abort
_abrt1      rts



uci_wait_abort
_wa1        lda CMD_IF_CONTROL
            and #CMD_ABORT
            bne _wa1
            rts

uci_clear_error
            lda #CMD_ERROR
            sta CMD_IF_CONTROL
            rts

uci_execute_from_ram
            ldx #<(uci_exec_ram_end - uci_exec_ram - 1)
_cpy        lda uci_exec_ram,x
            sta $0140,x
            dex
            bpl _cpy
            jmp $0140

uci_exec_ram:
            lda $01
            pha
            lda #$35 ; cannot use $34 here, for obvious reasons... I/O?
            sta $01
            lda #0
            sta UCI_PENDING_CMD
            lda #CMD_PUSH_CMD
            sta CMD_IF_CONTROL
_wbr1       lda CMD_IF_CONTROL
            and #CMD_STATE_BITS
            cmp #CMD_STATE_BUSY
            beq _wbr1
            pla
            sta $01
            rts
uci_exec_ram_end:

/* 
'stolen' from speeder.s
*/

do_uci_load:
        jmp     ulti_load ; exits with RTS, continues in persistent.s

new_load:
        sty     $93
        tya
        ldy     FA
        cpy     UCI_DEVICE
        beq     do_uci_load
        pla
        pla
        pla
        tay
        lda     #$F4
        pha
        lda     #$A6 ; F4A7 is normal continuation of kernal load routine
        pha
        jmp     _disable_rom_set_01


do_uci_save:
        jmp     ulti_save ; returns with RTS and continues in persistent.s

new_save:
        lda     FA
        cmp     UCI_DEVICE
        beq     do_uci_save
        pla
        pla
        pla
        tay
        lda #$f5
        pha
        lda #$ec      ; F5ED
        pha
        jmp      _disable_rom_set_01


print_searching:
        lda     $9D
        bpl     LA7A7
        ldy     #$0C ; "SEARCHING"
        jsr     print_kernal_string
        lda     $B7
        beq     LA7A7
        ldy     #$17 ; "FOR"
        jsr     print_kernal_string
LA796:  ldy     $B7
        beq     LA7A7
        ldy     #0
LA79C:  jsr     _load_FNADR_indy
        jsr     $E716 ; KERNAL: output character to screen
        iny
        cpy     $B7
        bne     LA79C
LA7A7:  rts

print_loading:
        ldy     #$49 ; "LOADING"
        lda     $93
        beq     LA7B3
        ldy     #$59 ; "VERIFYING"
        .byte   $2C
print_saving:
        ldy     #$51 ; "SAVING"
LA7B3:  bit     $9D
        bpl     LA7C4
print_kernal_string:
        lda     $F0BD,y ; KERNAL strings
        php
        and     #$7F
        jsr     $E716 ; KERNAL: output character to screen
        iny
        plp
        bpl     print_kernal_string ; until MSB set
LA7C4:  clc
        rts


/* ROM NESTING RESEARCH:
OPEN
Adds the currently set LFN to the logical file table and sends the filename if set.
In the process of modifying the logical file table, error 1, 2, and 6 can occur. In this case, CLRCHN ($ffcc) will be called
Also, when bit 6 of $9D is set, I/O error will be printed, using BSOUT ($ffd2)
Later on the serial bus, error 5 (device not present) may occur, which will in turn again call the same subfunctions.
Eventually all paths lead to RTS, as far as can be seen.

CLOSE
In case of a drive, it sends Listen, /Ex and Unlisten, clears C and returns. Status possibly gets set when a serial timeout occurs,
but errors are not directly generated.

CKIN
In case of a drive, it sends a TALK + SA over the serial bus.
May end in error 3 (file not open), or Error 5 (device not present). See Open. It always returns with RTS.
Interestingly, due to a bug, Error 5 does not occur when another device is present on the bus. And endless loop is the result.

CKOUT
In case of a drive, it sends LISTEN + SA over the serial bus
May end in error 3 (file not open), or Error 5 (device not present). See Open. It always returns with RTS.

BSIN
If the status is non-zero, it produces a carriage return. If status is zero, it takes a byte from the serial bus.
If an error occurs, Bit 1 is set of the status. Interestinly, the byte returned is not $0D (CR), but the byte that is left in the
accumulator after setting the IEC I/O lines high.  This function always returns with RTS.

BSOUT
Function always returns with RTS. It will set the status byte when a framing error or such occurs on the serial bus.

CLRCHN
If the current output channel is a drive, it sends the last buffered byte with EOI, and then unlisten.
If the current input channel is a drive, it sends untalk.  As we know, the low level serial I/O functions do not all other high level
routines; they do not jump to ERRORx. They simply set the status.


Summary: When jumping to the OPEN function, or the continuation of it in the kernal space, or the functions CKIN and CKOUT,
the ROM needs to be turned off. The function CANNOT be called from inside of the FC3 ROM!

*/
