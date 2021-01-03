use gb::cpu::Cpu as LR35902;

fn main() {
    let mut game_boy = LR35902::new_without_boot();

    game_boy.load_cartridge("bin/cpu_instrs.gb");

    loop {
        let pc = game_boy.register_pair(gb::cpu::RegisterPair::PC);
        let opcode = game_boy.fetch();
        let instruction = game_boy.decode(opcode);

        println!(
            "Addr: {:#06X} | Opcode: {:#04X} | Instr: {:X?}",
            pc, opcode, instruction
        );

        game_boy.execute(instruction);
    }
}
