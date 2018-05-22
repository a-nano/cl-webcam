# Cl-Webcam - web camera api

## Usage

```common-lisp

;; to load escapi library
(load-library)
;=> t

;; check web camera devices
(get-capture-device-name-list)
;=> ((0 . "TOSHIBA Web Camera - HD") (1 . "Logicool Webcam C930e"))

;; single shot capture on device 0
(single-capture 0)
;=> #(46 32 135 120 56 62 32 43 64 102 200 54 31 31 65 87 ...)

;; to save jpeg file (use cl-jpeg)
(ql:quickload :cl-jpeg)
(cl-jpeg:encode-image "foo.jpg" (single-capture 0) 3 480 640)


## Installation

## Author

* Akihide Nano (an74abc@gmail.com)

## Copyright

Copyright (c) 2018 Akihide Nano (an74abc@gmail.com)

## License

Licensed under the LLGPL License.
