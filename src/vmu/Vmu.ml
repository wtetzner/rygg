
module Asm = struct
  include Asm
  module Parser = struct
    let assemble = Asm_Parser.assemble
  end
end


