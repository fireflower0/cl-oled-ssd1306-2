(ql:quickload "cffi")
(defpackage :cl-cffi
  (:use :cl
        :cffi))
(in-package :cl-cffi)

(load "libwiringPi.lisp" :external-format :utf-8)
(load "8x8-font.lisp" :external-format :utf-8)

(defconstant +i2c-addr+ #X3C)
(defvar *fd* (wiringPiI2CSetup +i2c-addr+))

;;; Initialize Command
(defconstant +ssd1306-disp-on+           #XAF)
(defconstant +ssd1306-disp-off+          #XAE)
(defconstant +ssd1306-set-disp-clk-div+  #XD5)
(defconstant +ssd1306-set-multiplex+     #XA8)
(defconstant +ssd1306-set-disp-offset+   #XD3)
(defconstant +ssd1306-set-start-line+    #X40)
(defconstant +ssd1306-seg-re-map+        #XA0)
(defconstant +ssd1306-com-scan-inc+      #XC0)
(defconstant +ssd1306-com-scan-dec+      #XC8)
(defconstant +ssd1306-set-com-pins+      #XDA)
(defconstant +ssd1306-set-contrast+      #X81)
(defconstant +ssd1306-disp-allon-resume+ #XA4)
(defconstant +ssd1306-normal-disp+       #XA6)
(defconstant +ssd1306-charge-pump+       #X8D)
(defconstant +ssd1306-deactivate-scroll+ #X2E)
(defconstant +ssd1306-set-mem-addr-mode+ #X20)
(defconstant +ssd1306-set-column-addr+   #X21)
(defconstant +ssd1306-set-page-addr+     #X22)

;;; Control Byte
;; Co bit = 0 (continue), D/C# = 0 (command)
(defconstant +ssd1306-command+           #X00)
;; Co bit = 0 (continue), D/C# = 1 (data)
(defconstant +ssd1306-data+              #X40)
;; Co bit = 1 (One command only), D/C# = 0 (command)
(defconstant +ssd1306-control+           #X80)

;;; OLED Info
(defconstant +ssd1306-lcd-width+         128)
(defconstant +ssd1306-lcd-height+        64)

(defun ssd1306-command (value)
  (wiringPiI2CWriteReg8 *fd* +ssd1306-command+ value))

(defun ssd1306-data (value)
  (wiringPiI2CWriteReg8 *fd* +ssd1306-data+ value))

(defun ssd1306-control (value)
  (wiringPiI2CWriteReg8 *fd* +ssd1306-control+ value))

(defun ssd1306-init ()
  ;; Display Off                         #XAE
  (ssd1306-command +ssd1306-disp-off+)
  
  ;; Set MUX Raio                        #XA8, #X3F(63)
  (ssd1306-command +ssd1306-set-multiplex+)
  (ssd1306-command (1- +ssd1306-lcd-height+))

  ;; Set Display Offset                  #XD3, #X00
  (ssd1306-command +ssd1306-set-disp-offset+)
  (ssd1306-command #X00)   ; no offset

  ;; Set Display Start Line              #X40
  (ssd1306-command +ssd1306-set-start-line+)

  ;; Set Segment re-map                  #XA0/#XA1
  (ssd1306-command +ssd1306-seg-re-map+)

  ;; Set COM Output Scan Direction       #XC0/#XC8
  (ssd1306-command +ssd1306-com-scan-inc+)

  ;; Set COM Pins hardware configuration #XDA, #X02
  (ssd1306-command +ssd1306-set-com-pins+)
  (ssd1306-command #X02)

  ;; Set Contrast Control                #X81, #X7F
  (ssd1306-command +ssd1306-set-contrast+)
  (ssd1306-command #X7F)

  ;; Disable Entire Display On           #XA4
  (ssd1306-command +ssd1306-disp-allon-resume+)

  ;; Set Normal Display                  #XA6
  (ssd1306-command +ssd1306-normal-disp+)

  ;; Set Osc Frequency                   #XD5, #X80
  (ssd1306-command +ssd1306-set-disp-clk-div+)
  (ssd1306-command #X80)   ; the suggested ratio 0x80

  ;; Deactivate scroll                   #X2E
  (ssd1306-command +ssd1306-deactivate-scroll+)

  ;; Set Memory Addressing Mode          #X20, #X10
  (ssd1306-command +ssd1306-set-mem-addr-mode+)
  (ssd1306-command #X10)   ; Page addressing Mode

  ;; Set Column Address                  #X21
  (ssd1306-command +ssd1306-set-column-addr+)
  (ssd1306-command 0)      ; Column Start Address
  (ssd1306-command 127)    ; Column Stop Address

  ;; Set Page Address                    #X22
  (ssd1306-command +ssd1306-set-page-addr+)
  (ssd1306-command 0)      ; Vertical start position
  (ssd1306-command 7)      ; Vertical end position

  ;; Enable change pump regulator        #X8D, #X14
  (ssd1306-command +ssd1306-charge-pump+)
  (ssd1306-command #X14)

  ;; Display On                          #XAF
  (ssd1306-command +ssd1306-disp-on+))

(defun display-black ()
  (dotimes (i 8)
    (ssd1306-control (logior #XB0 i))   ; set page start address
    (dotimes (j 16)
      (dotimes (k 8)
        (ssd1306-data #X00)))))

(defun display-fontmap (str)
  (ssd1306-command (logior #XB0 4))     ; Set page start address
  (ssd1306-command #X21)                ; Set column address
  (ssd1306-command 50)                  ; Start column address
  (ssd1306-command 127)                 ; Stop Colunm address
  (let (char-list)
    (setf char-list (coerce str 'list))
    (dolist (char char-list)
      (dotimes (i 8)
        (ssd1306-data (aref font-8x8 (- (char-code char) #X20) i))))))

;; Horizontal Scroll
(defun h-scroll-display ()
  (ssd1306-command #X2E)                ; For configuration, once off the scroll
  (ssd1306-command #X26)                ; Horizontal scroll set. 0x27=Reverse direction.
  (ssd1306-command #X00)                ; Dummy byte
  (ssd1306-command (logior #X00 0))     ; Define start page address.
  (ssd1306-command (logior #X00 7))     ; Set time interval pattern.
  (ssd1306-command (logior #X00 7))     ; Define end page address.
  (ssd1306-command #X00)                ; Dummy byte
  (ssd1306-command #XFF)                ; Dummy byte
  (ssd1306-command #X2F))               ; Activate scroll

;; Vertical and Horizontal Scroll
(defun vh-scroll-display ()
  (ssd1306-command #X2E)                ; For configuration, once off the scroll
  (ssd1306-command #X2A)                ; V and H scroll setup. 0x29=Reverse.
  (ssd1306-command #X00)                ; Dummy byte
  (ssd1306-command (logior #X00 0))     ; Define start page address.
  (ssd1306-command (logior #X00 7))     ; Set time interval pattern.
  (ssd1306-command (logior #X00 7))     ; Define and page address.
  (ssd1306-command (logior #X00 63))    ; Vertical scrolling offset.
  (ssd1306-command #X2F))               ; Activate scroll

(defun main ()
  (delay 1000)       ; Wait until the ESP-WROOM-02 (ESP8266) starts up.
  
  (ssd1306-init)
  (display-black)
  (display-fontmap "Hello")
  (h-scroll-display)

  (delay 5000)

  (ssd1306-init)
  (display-black)
  (display-fontmap "world")
  (vh-scroll-display))

(main)
