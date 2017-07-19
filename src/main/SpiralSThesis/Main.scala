package SpiralSThesis


object MainThesis extends App {
  val dsl = new CorewGlue(testsize = 4, //2^testsize
    radix_choice = radix,
    static_size = Some(16),
    interleaved = true,
    thread = false,
    base_default = 16,
    twid_inline = false,
    twid_default_precomp = false,
    validate = true,
    inplace = false
  )
  dsl.codeexport("F:\\Phd\\git\\code\\SpiralSTarget\\src\\main\\Test.scala")
  dsl.graphexport(name = "statsize_codelet_int_statinstride.dot")
  val f = dsl.compile()
  val perf = f();
  true
}