; ----------------------------------------------------------------
; Common drive code
; ----------------------------------------------------------------
; The BASIC extension and fast format call into this.
.feature c_comments

.include "kernal.i"

; from wrapper
.import disable_rom_jmp_error

; from basic
.import set_drive

.global print_line_from_drive
.global check_iec_error
.global cmd_channel_listen
.global listen_second
.global command_channel_talk
.global talk_second
.global m_w_and_m_e
.global listen_6F_or_error
.global listen_or_error
.global device_not_present

.import ckout_known_fasa
.import ckin_known_fasa
.import new_bsin2
.import new_bsout2
.import new_clrch2

.segment "drive"

; -----------------------------------------
print_line_from_drive:
        jsr     new_bsin2 ; new_bsin2 ; IECIN
        jsr     $E716 ; output character to the screen
        cmp     #CR
        bne     print_line_from_drive
        jmp     new_clrch2; new_clrch2 ; UNTALK

check_iec_error:
        jsr     command_channel_talk
        jsr     new_bsin2 ; IECIN
        tay
L8124:  jsr     new_bsin2 ; IECIN
        cmp     #CR ; skip message
        bne     L8124
        tay
        jsr     new_clrch2 ; UNTALK
        cpy     #'0'
        rts

cmd_channel_listen:
        lda     #$6F
listen_second:
        sta     SA
        jsr     set_drive
        jsr     ckout_known_fasa
        lda     ST
        rts

command_channel_talk:
        lda     #$6F
talk_second:
        sta     SA
        jsr     set_drive
        jsr     ckin_known_fasa
        rts

m_w_and_m_e:
        sta     $C3
        sty     $C4
        ldy     #0
L8154:  lda     #'W'
        jsr     send_m_dash
        tya
        jsr     new_bsout2 ; IECOUT
        txa
        jsr     new_bsout2 ; IECOUT
        lda     #' '
        jsr     new_bsout2 ; IECOUT
L8166:  lda     ($C3),y
        jsr     new_bsout2 ; IECOUT
        iny
        tya
        and     #$1F
        bne     L8166
        jsr     UNLSTN
        tya
        bne     L8154
        inc     $C4
        inx
        cpx     $93
        bcc     L8154
        lda     #'E'
send_m_dash:
        pha
        jsr     listen_6F_or_error
        lda     #'M'
        jsr     new_bsout2 ; IECOUT
        lda     #'-'
        jsr     new_bsout2 ; IECOUT
        pla
        jmp     new_bsout2 ; IECOUT

listen_6F_or_error:
        lda     #$6F
listen_or_error:
        jsr     listen_second
        lda     ST
        bmi     device_not_present
        rts

device_not_present:
        ldx     #5 ; "DEVICE NOT PRESENT"
        jmp     disable_rom_jmp_error

