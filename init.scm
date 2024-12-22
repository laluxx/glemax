(use-modules (glemax))

;; Create and switch buffers
(buffer-new "*porcodio*" "" "default-font")
(buffer-switch "*porcodio*")

;; Split windows
(window-split-horizontal)
(window-focus-next 1)

;; Show messages
(message "Glemax initialized!")
