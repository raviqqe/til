%struct = type {double,double,double,double,double,double,double,double,double,double,double,double,double,double,double,double}

define double @foo(%struct %0) {
  %a = extractvalue %struct %0, 0
  %b = extractvalue %struct %0, 15
  %c = fadd double %a, %b

  ret double %c
}
