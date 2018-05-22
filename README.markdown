# Cl-Webcam - web camera api

## Usage

For use single shot capture (slowly but safety).
```common-lisp

;; To load escapi library.
(load-library)
;=> t

;; Check web camera devices.
(get-capture-device-name-list)
;=> ((0 . "TOSHIBA Web Camera - HD") (1 . "Logicool Webcam C930e"))

;; Single shot capture on device 0 (return vector of rgb image).
(single-capture 0)
;=> #(46 32 135 120 56 62 32 43 64 102 200 54 31 31 65 87 ...)

;; To save jpeg file (use cl-jpeg).
(ql:quickload :cl-jpeg)
(cl-jpeg:encode-image "foo.jpg" (single-capture 0) 3 480 640)

```

Customized use (unsafety but fast).
```common-lisp

;; To load escapi library.
(load-library)
;=> t

;; Check web camera devices.
(get-capture-device-name-list)
;=> ((0 . "TOSHIBA Web Camera - HD") (1 . "Logicool Webcam C930e"))

;; Make instance of parameter's structure.
(defvar *param* (make-capture-parameter 480 640 10.0))

;; Init capture device.
(init-capture 0 *param*)
;=> t

;; Do capture.
(do-capture 0)
;=> t

;; Confirm whether capture is finish.
(wait-capture-done 0)
;=> DONE

;; Get buffer (default is vector of rgb image).
(get-buffer *param*)
;=> #(46 32 135 120 56 62 32 43 64 102 200 54 31 31 65 87 ...)


;;               ;;
;; Do something! ;;
;;               ;;

;; Close capture dvice.
(deinit-capture 0)
;=> t

;; Do free instance of parameter's structure.
(clear-capture-parameters *param*)
;=> t

```

## Installation
1. Download `escapi.dll' and set Path your environment.
(http://sol.gfxile.net/zip/escapi3.zip)

2. Clone to your local-projects.
```common-lisp
cd $HOME/quicklisp/local-projects
git clone https://github.com/a-nano/cl-webcam.git
```

3. Start your lisp. Then, jsut:
```common-lisp
(ql:quickload :cl-webcam)
```

4. To run test system.
```common-lisp
(asdf:test-system :cl-webcam)
```

## Author

* Akihide Nano (an74abc@gmail.com)

## Copyright

Copyright (c) 2018 Akihide Nano (an74abc@gmail.com)

## License

Licensed under the LLGPL License.
