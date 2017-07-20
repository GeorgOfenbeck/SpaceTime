package SpiralSThesis


object MainThesis {
  def main(args: Array[String]): Unit = {
    val defradix: Map[Int, Int] = Map(
      4 -> 2
      , 8 -> 2
      , 16 -> 2
      , 32 -> 2
      , 64 -> 2
      , 128 -> 2
      , 256 -> 2
      , 512 -> 2
      , 1024 -> 2
      , 2048 -> 2
      , 4096 -> 2
      , 8192 -> 2
      , 16384 -> 2
      , 32768 -> 2
      , 65536 -> 2
    ).withDefaultValue(256)

    val dsl = new CorewGlue(testsize = 4, //2^testsize
      radix_choice = defradix,
      static_size = Some(16),
      interleaved = true,
      thread = false,
      base_default = 16,
      twid_doinline = false,
      twid_default_precomp = false,
      validate = true,
      inplace = false
    )
    dsl.codeexport("F:\\Phd\\git\\code\\SpiralSTarget\\src\\main\\Testdotty.scala")
    dsl.graphexport(name = "statsize_codelet_int_statinstride.dot")
    //val f = dsl.compile()
    //val perf = f();
    true
  }
}