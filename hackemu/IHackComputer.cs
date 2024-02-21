namespace hackemu;

public interface IHackComputer
{
    short A { get; }
    short D { get; }
    short M { get; }
    ushort PC { get; }
    short Memory(ushort index);
    void SetMemory(ushort index, short value);
    void ComputeCycles(int count);

    /// <summary>
    /// Computes current instruction
    /// </summary>
    /// <exception cref="ArgumentOutOfRangeException"></exception>
    void ComputeNext();
}