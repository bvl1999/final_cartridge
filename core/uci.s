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

.import print_searching
.import print_loading
.import print_saving


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
        DEVFROM    = $99
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

ld2         jsr print_searching
            ldx #UCI_CMD_LOADSU
            jsr uci_setup_cmd
            ldy #LOADADDR
            jsr uci_setup_range
            jsr uci_filename
            jsr uci_execute
            lda CMD_IF_STATUS
            jsr uci_ack ; restores A
            beq ld3 ; all OK when zero
            lda #4  ; File not found error
            sec
            rts

ld3         jsr print_loading

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
            clc
            rts

_verify_err jsr uci_ack ; restores A
            lda #$10
            ora STATUS
            sta STATUS
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

sv2         jsr print_saving
            ldx #UCI_CMD_SAVE
            jsr uci_setup_cmd
            ldy #SAVEADDR
            jsr uci_setup_range
            jsr uci_filename
            jsr uci_execute_from_ram
            lda CMD_IF_STATUS
            beq sv3 ; all OK when zero

            jsr uci_ack
            sec
            lda #7 ; not output file :)
            rts

sv3         jsr uci_ack
            clc
            rts


; ----------------------------------------------------------------
new_open2:
; $FFC0   
; OPEN. Open file. (Must call SETLFS and SETNAM beforehand.)
; Input: –
; Output: –
; Used registers: A, X, Y.
; Real address: ($031A), $F34A.
; 

            lda DEVNUM
            cmp UCI_DEVICE
            beq myopen
op1         jmp (OPEN_ORIG)

myopen      lda CMD_IF_COMMAND
            cmp #UCI_IDENTIFIER
            bne error5

            ; The following is an optimized copy of the kernal code at F34A, because
            ; it is common code, which should always be executed, but it
            ; cannot be vectored.

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

myclose     lda CMD_IF_COMMAND
            cmp #UCI_IDENTIFIER
            bne cl2
            pla ; restore stack, but we don't need a

            ldx #UCI_CMD_CLOSE
            jsr uci_setup_cmd
            jsr uci_execute
            jsr uci_ack
            clc
            rts

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
            lda DEVNUM
            cmp UCI_DEVICE
            beq _my_chkin
            jmp CHKIN_CONTINUED    ; continue at stock kernal location

_my_chkin   sta DEVFROM
do_chkin    ldx #UCI_CMD_CHKIN
            jsr uci_setup_cmd
            jsr uci_execute
            clc
            rts

.segment "ultimate2"

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
            jmp (CHRIN_ORIG)

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
            jmp (GETIN_ORIG)



; $FFCC
; CLRCHN. Close default input/output files (for serial bus, send UNTALK and/or UNLISTEN); restore default input/output to keyboard/screen.
; Input: –
; Output: –
; Used registers: A, X.
; Real address: ($0322), $F333.
;
; This is called as a subroutine from FC3 code
ulti_clrchn:
            lda DEVNUM
            cmp UCI_DEVICE
            beq _my_clrchn
_clr2       rts

_my_clrchn  lda CMD_IF_COMMAND
            cmp #UCI_IDENTIFIER
            bne _clr2
            jmp uci_abort

ulti_ckout:
            lda #0
            sta UCI_OUTLEN
            ldx #UCI_CMD_CHKOUT
            clc
            jmp uci_setup_cmd       ; do not execute command, because we are waiting for data now


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
            lda NAMELEN
            beq _fn2
            ldy #$00
_fn1        jsr _load_FNADR_indy
            sta CMD_IF_COMMAND
            iny
            cpy NAMELEN
            bne _fn1
_fn2        rts

uci_execute lda #CMD_PUSH_CMD
            sta CMD_IF_CONTROL

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

uci_abort   lda CMD_IF_CONTROL
            and #CMD_STATE_DATA
            bne _abrt1
            ; Not in data state, but may be in command state
            ; So send command, even if it may be an empty command
            lda #CMD_PUSH_CMD
            sta CMD_IF_CONTROL
            jsr uci_wait_busy
            jmp uci_ack
_abrt1      lda #CMD_ABORT
            sta CMD_IF_CONTROL
            jmp uci_wait_abort

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
            sei
            lda #$35 ; cannot use $34 here, for obvious reasons... I/O?
            sta $01
            lda #CMD_PUSH_CMD
            sta CMD_IF_CONTROL
_wbr1       lda CMD_IF_CONTROL
            and #CMD_STATE_BITS
            cmp #CMD_STATE_BUSY
            beq _wbr1
            pla
            sta $01
            cli
            rts
uci_exec_ram_end:

