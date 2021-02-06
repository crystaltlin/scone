(asdf:load-system "py4cl")

(py4cl:python-eval "[i**2 for i in range(5)]") ; => #(0 1 4 9 16)
