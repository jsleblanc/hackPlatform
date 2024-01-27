namespace hackemu;

public class HackVirtualMachine(HackComputer hackComputer)
{
    public short SP => hackComputer.Memory(0);
    public short LCL => hackComputer.Memory(1);
    public short ARG => hackComputer.Memory(2);
    public short THIS => hackComputer.Memory(3);
    public short THAT => hackComputer.Memory(4);
}