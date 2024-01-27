namespace hackemu;

public class HackVirtualMachine(HackComputer hackComputer) : IHackComputer
{
    public short SP => hackComputer.Memory(0);
    public short LCL => hackComputer.Memory(1);
    public short ARG => hackComputer.Memory(2);
    public short THIS => hackComputer.Memory(3);
    public short THAT => hackComputer.Memory(4);

    public short R0 => hackComputer.Memory(0);
    public short R1 => hackComputer.Memory(1);
    public short R2 => hackComputer.Memory(2);
    public short R3 => hackComputer.Memory(3);
    public short R4 => hackComputer.Memory(4);
    public short R5 => hackComputer.Memory(5);
    public short R6 => hackComputer.Memory(6);
    public short R7 => hackComputer.Memory(7);
    public short R8 => hackComputer.Memory(8);
    public short R9 => hackComputer.Memory(9);
    public short R10 => hackComputer.Memory(0xA);
    public short R11 => hackComputer.Memory(0xB);
    public short R12 => hackComputer.Memory(0xC);
    public short R13 => hackComputer.Memory(0xD);
    public short R14 => hackComputer.Memory(0xE);
    public short R15 => hackComputer.Memory(0xF);
    
    public short A => hackComputer.A;
    public short D => hackComputer.D;
    public short M => hackComputer.M;
    public short PC => hackComputer.PC;
    
    public short Memory(short index) => hackComputer.Memory(index);
    public void SetMemory(short index, short value) => hackComputer.SetMemory(index, value);
    public void ComputeCycles(int count) => hackComputer.ComputeCycles(count);

    public void ComputeNext() => hackComputer.ComputeNext();
}