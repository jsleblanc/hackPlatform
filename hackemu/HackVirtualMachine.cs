namespace hackemu;

public class HackVirtualMachine(HackComputer hackComputer) : IHackComputer
{
    //for debugging
    private readonly HackComputer _hackComputer = hackComputer;
    
    public short SP => _hackComputer.Memory(0);
    public short LCL => _hackComputer.Memory(1);
    public short ARG => _hackComputer.Memory(2);
    public short THIS => _hackComputer.Memory(3);
    public short THAT => _hackComputer.Memory(4);

    public short R0 => _hackComputer.Memory(0);
    public short R1 => _hackComputer.Memory(1);
    public short R2 => _hackComputer.Memory(2);
    public short R3 => _hackComputer.Memory(3);
    public short R4 => _hackComputer.Memory(4);
    public short R5 => _hackComputer.Memory(5);
    public short R6 => _hackComputer.Memory(6);
    public short R7 => _hackComputer.Memory(7);
    public short R8 => _hackComputer.Memory(8);
    public short R9 => _hackComputer.Memory(9);
    public short R10 => _hackComputer.Memory(0xA);
    public short R11 => _hackComputer.Memory(0xB);
    public short R12 => _hackComputer.Memory(0xC);
    public short R13 => _hackComputer.Memory(0xD);
    public short R14 => _hackComputer.Memory(0xE);
    public short R15 => _hackComputer.Memory(0xF);
    
    public short A => _hackComputer.A;
    public short D => _hackComputer.D;
    public short M => _hackComputer.M;
    public ushort PC => _hackComputer.PC;
    
    public short Memory(ushort index) => _hackComputer.Memory(index);
    public void SetMemory(ushort index, short value) => _hackComputer.SetMemory(index, value);
    public void ComputeCycles(int count) => _hackComputer.ComputeCycles(count);

    public void ComputeNext() => _hackComputer.ComputeNext();
}