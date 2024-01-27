namespace hackemu;

public interface IHackComputer
{
    short A { get; }
    short D { get; }
    short M { get; }
    short PC { get; }
    short Memory(short index);
    void SetMemory(short index, short value);
    void ComputeCycles(int count);

    /// <summary>
    /// Computes current instruction
    /// </summary>
    /// <exception cref="ArgumentOutOfRangeException"></exception>
    void ComputeNext();
}