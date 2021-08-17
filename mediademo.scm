;;
;; Media Demo
;;

(define microphoneCanvas (canvas "Microphone"  300 300))
(@= microphoneCanvas 'user-media { "audio": true })
