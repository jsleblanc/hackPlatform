// ReSharper disable InconsistentNaming
namespace hackemu;

public class HackVirtualMachine(HackComputer computer)
{
    //for debugging    
    private readonly HackComputer _computer = computer;
    
    private const ushort SEGMENT_LOCAL = 1;
    private const ushort SEGMENT_ARG = 2;
    private const ushort SEGMENT_THIS = 3;
    private const ushort SEGMENT_THAT = 4;
    private const ushort SEGMENT_STATIC_BASE = 16;

    public short SP => _computer.Memory(0);
    public short LCL => _computer.Memory(SEGMENT_LOCAL);
    public short ARG => _computer.Memory(SEGMENT_ARG);
    public short THIS => _computer.Memory(SEGMENT_THIS);
    public short Pointer_0 => _computer.Memory((ushort)THIS);
    public short THAT => _computer.Memory(SEGMENT_THAT);
    public short Pointer_1 => _computer.Memory((ushort)THAT);

    public short R0 => _computer.Memory(0);
    public short R1 => _computer.Memory(1);
    public short R2 => _computer.Memory(2);
    public short R3 => _computer.Memory(3);
    public short R4 => _computer.Memory(4);
    public short R5 => _computer.Memory(5);
    public short R6 => _computer.Memory(6);
    public short R7 => _computer.Memory(7);
    public short R8 => _computer.Memory(8);
    public short R9 => _computer.Memory(9);
    public short R10 => _computer.Memory(0xA);
    public short R11 => _computer.Memory(0xB);
    public short R12 => _computer.Memory(0xC);
    public short R13 => _computer.Memory(0xD);
    public short R14 => _computer.Memory(0xE);
    public short R15 => _computer.Memory(0xF);

    public short TopOfStack => _computer.Memory((ushort)(SP - 1));
    public short Stack(short offset) => _computer.Memory((ushort)(SP - 1 - offset));
    
    public void SetThisBase(short address) => _computer.SetMemory(SEGMENT_THIS, address);
    public void SetThatBase(short address) => _computer.SetMemory(SEGMENT_THAT, address);

    public void SetPointer0(short value) => _computer.SetMemory((ushort)THIS, value);
    public void SetPointer1(short value) => _computer.SetMemory((ushort)THAT, value);
    
    public void SetLocalSegmentBase(short address) => _computer.SetMemory(SEGMENT_LOCAL, address);
    public short LocalSegment(short index) => _computer.Memory((ushort)(LCL + index));
    
    public void SetArgumentSegmentBase(short address) => _computer.SetMemory(SEGMENT_ARG, address);
    public void SetArgumentSegment(short index, short value) => _computer.SetMemory((ushort)(ARG + index), value);
    public short ArgumentSegment(short index) => _computer.Memory((ushort)(ARG + index));

    public void SetThisSegmentBase(short address) => _computer.SetMemory(SEGMENT_THIS, address);
    public short ThisSegment(short index) => _computer.Memory((ushort)(THIS + index));

    public void SetThatSegmentBase(short address) => _computer.SetMemory(SEGMENT_THAT, address);
    public short ThatSegment(short index) => _computer.Memory((ushort)(THAT + index));

    public void SetStaticSegment(short index, short value) => _computer.SetMemory((ushort)(SEGMENT_STATIC_BASE + index), value);
    public short StaticSegment(short index) => _computer.Memory((ushort)(SEGMENT_STATIC_BASE + index));
    
    public short A => _computer.A;
    public short D => _computer.D;
    public short M => _computer.M;
    public ushort PC => _computer.PC;
    
    public short Memory(ushort index) => _computer.Memory(index);
    public void SetMemory(ushort index, short value) => _computer.SetMemory(index, value);
    public void ComputeCycles(int count) => _computer.ComputeCycles(count);

    public void ComputeNext() => _computer.ComputeNext();

    public HackVirtualMachine(IEnumerable<string> programCode) : this(new HackComputer(programCode))
    {
    }
}