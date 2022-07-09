%struct = type [8 x double]

define double @foo(%struct %0) {
  %x = extractvalue %struct %0, 7

  ret double %x
}
