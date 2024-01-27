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
    /// Runs computation until infinite loop detection kicks in or provided cycle limit is reached
    /// </summary>
    /// <param name="cycleLimit"></param>
    /// <returns>Number of cycles executed</returns>
    int ComputeUntilFinishedWithLimit(int cycleLimit = 1000);

    /// <summary>
    /// Computes current instruction
    /// </summary>
    /// <returns>True if program continues, false if infinite loop detected</returns>
    /// <exception cref="ArgumentOutOfRangeException"></exception>
    bool ComputeNext();
}