
module Asm = struct
  include Asm
  module Parser = struct
    let assemble = Asm_Parser.assemble
  end
end

module Disasm = struct
  include Disasm
end

module Lir = Lir
