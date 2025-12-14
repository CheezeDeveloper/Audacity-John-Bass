;nyquist plug-in
;version 4
;type process
;name "John Bass 2.0"
;action "Cooking Audio..."
;author "Reaper Port + Upgrades"
;copyright "2025 CheezeDev/CheezeDeveloper"

;; --- THE MENU ---
;control mode "Select Preset" choice "Manual Mode,Mild Fry,Deep Fried Meme,Broken Android,THE NUCLEAR OPTION,Wide & Heavy,Swim44" 0

;; --- THE CONTROLS ---
;control u_amt "Destruction Amount" float "%" 0 0 100
;control u_sub "Sub-Bass Injection" float "%" 0 0 100
;control u_width "Stereo Widening" float "%" 0 0 100
;control u_lofi "Low Quality Mode" choice "Off,On" 0
;control u_beep "The Ear Piercer (Beep)" choice "Off,On" 0

;; --- INTERNAL FUNCTION ---
(defun process-audio (sig mode amt sub width lofi beep)
  (let ((drive-amt 0)
        (sub-amt 0)
        (width-amt 0)
        (is-lofi 0)
        (is-beep 0)
        ;; Internal calculation vars
        (drive-val 0)
        (sub-sig 0)
        (sig-w-beep 0)
        (boosted 0)
        (crushed 0)
        (widened 0)
        (output 0))

    ;; --- PRESET BRAIN ---
    ;; Note: Audacity sliders won't visually move, but the math changes here.
    (cond 
      ;; 0: Manual Mode (Uses your slider settings)
      ((= mode 0) 
       (setf drive-amt amt) 
       (setf sub-amt sub) 
       (setf width-amt width) 
       (setf is-lofi lofi) 
       (setf is-beep beep))
       
      ;; 1: Mild Fry
      ((= mode 1) (setf drive-amt 25) (setf sub-amt 10) (setf width-amt 0) (setf is-lofi 0) (setf is-beep 0))
       
      ;; 2: Deep Fried Meme
      ((= mode 2) (setf drive-amt 60) (setf sub-amt 40) (setf width-amt 20) (setf is-lofi 0) (setf is-beep 0))
       
      ;; 3: Broken Android
      ((= mode 3) (setf drive-amt 80) (setf sub-amt 0) (setf width-amt 0) (setf is-lofi 1) (setf is-beep 0))
       
      ;; 4: NUCLEAR
      ((= mode 4) (setf drive-amt 100) (setf sub-amt 100) (setf width-amt 50) (setf is-lofi 1) (setf is-beep 1))

      ;; 5: Wide & Heavy (New Cool Preset)
      ((= mode 5) (setf drive-amt 40) (setf sub-amt 80) (setf width-amt 100) (setf is-lofi 0) (setf is-beep 0))
       
      ;; 6: Swim44 (Silence)
      ((= mode 6) (setf drive-amt 0) (setf sub-amt 0) (setf width-amt 0) (setf is-lofi 0) (setf is-beep 0))
    )

    ;; --- DSP STEP 1: CALCULATE DRIVE ---
    (setf drive-val (+ 1.0 (* drive-amt 0.5)))

    ;; --- DSP STEP 2: SUB BASS INJECTION ---
    ;; Extracts low end, distorts it, and mixes it back in
    (if (> sub-amt 0)
        (let ((low-end (lp (scale 2.0 sig) 100))) 
          ;; Add the sub to the main signal
          (setf sig (sum sig (scale (* sub-amt 0.02) low-end))))
        (setf sig sig))

    ;; --- DSP STEP 3: THE BEEP ---
    (if (= is-beep 1)
        (setf sig-w-beep (sum sig (scale 0.2 (hzosc 3000))))
        (setf sig-w-beep sig))

    ;; --- DSP STEP 4: BOOST ---
    (setf boosted (scale drive-val sig-w-beep))

    ;; --- DSP STEP 5: BITCRUSH ---
    (if (= is-lofi 1)
        (setf crushed (quantize boosted 8))
        (setf crushed boosted))

    ;; --- DSP STEP 6: STEREO WIDENING ---
    ;; Only applies if the track is stereo (array)
    (if (and (arrayp crushed) (> width-amt 0))
        (let* ((L (aref crushed 0))
               (R (aref crushed 1))
               (mid (scale 0.5 (sum L R)))
               (side (scale 0.5 (diff L R)))
               (width-factor (+ 1.0 (* width-amt 0.02))))
          ;; Boost the "Side" channel to widen
          (setf widened (vector 
                         (sum mid (scale width-factor side))
                         (diff mid (scale width-factor side)))))
        ;; If mono or width is 0, pass through
        (setf widened crushed))

    ;; --- DSP STEP 7: HARD CLIPPER ---
    (setf output (clip widened 1.0))

    ;; --- FINAL RETURN (Check for Swim44) ---
    (if (= mode 6)
        (scale 0.0 sig)
        output)
  )
)

;; RUN THE PLUGIN
(multichan-expand #'process-audio *track* mode u_amt u_sub u_width u_lofi u_beep)