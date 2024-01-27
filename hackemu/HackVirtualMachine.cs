namespace hackemu;

public class HackVirtualMachine(HackComputer hackComputer) : IHackComputer
{
    public short SP => hackComputer.Memory(0);
    public short LCL => hackComputer.Memory(1);
    public short ARG => hackComputer.Memory(2);
    public short THIS => hackComputer.Memory(3);
    public short THAT => hackComputer.Memory(4);
    
    public short A => hackComputer.A;
    public short D => hackComputer.D;
    public short M => hackComputer.M;
    public short PC => hackComputer.PC;
    
    public short Memory(short index) => hackComputer.Memory(index);
    public void SetMemory(short index, short value) => hackComputer.SetMemory(index, value);
    public void ComputeCycles(int count) => hackComputer.ComputeCycles(count);

    public int ComputeUntilFinishedWithLimit(int cycleLimit = 1000) =>
        hackComputer.ComputeUntilFinishedWithLimit(cycleLimit);

    public bool ComputeNext() => hackComputer.ComputeNext();
}