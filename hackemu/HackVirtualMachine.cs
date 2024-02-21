namespace hackemu;

public class HackVirtualMachine(IHackComputer computer) : IHackComputer
{
    //for debugging

    public short SP => computer.Memory(0);
    public short LCL => computer.Memory(1);
    public short ARG => computer.Memory(2);
    public short THIS => computer.Memory(3);
    public short THAT => computer.Memory(4);

    public short R0 => computer.Memory(0);
    public short R1 => computer.Memory(1);
    public short R2 => computer.Memory(2);
    public short R3 => computer.Memory(3);
    public short R4 => computer.Memory(4);
    public short R5 => computer.Memory(5);
    public short R6 => computer.Memory(6);
    public short R7 => computer.Memory(7);
    public short R8 => computer.Memory(8);
    public short R9 => computer.Memory(9);
    public short R10 => computer.Memory(0xA);
    public short R11 => computer.Memory(0xB);
    public short R12 => computer.Memory(0xC);
    public short R13 => computer.Memory(0xD);
    public short R14 => computer.Memory(0xE);
    public short R15 => computer.Memory(0xF);
    
    public short A => computer.A;
    public short D => computer.D;
    public short M => computer.M;
    public ushort PC => computer.PC;
    
    public short Memory(ushort index) => computer.Memory(index);
    public void SetMemory(ushort index, short value) => computer.SetMemory(index, value);
    public void ComputeCycles(int count) => computer.ComputeCycles(count);

    public void ComputeNext() => computer.ComputeNext();

    public HackVirtualMachine(IEnumerable<string> programCode) : this(new HackComputer(programCode))
    {
    }
}