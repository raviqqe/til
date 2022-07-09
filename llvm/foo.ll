%struct = type {double,double,double}

define double @foo(%struct %0) {
  %x = extractvalue %struct %0, 1

  ret double %x
}
