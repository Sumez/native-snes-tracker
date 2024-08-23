using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Patcher.ViewModels;
public class Patch
{
	public SampleFileCollection AvailableSamples { get; } = new();
}
