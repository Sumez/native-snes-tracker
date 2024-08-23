using Avalonia.Controls;
using Patcher.ViewModels;
using System.Collections.Generic;

namespace Patcher;

public partial class ImportSamples : Window
{
	public ImportSamples()
	{
		InitializeComponent();
		DataContext = _sampleFiles = new SampleFileCollection();
	}
	public ImportSamples(List<SampleFile> sampleFiles) : this()
	{
		DataContext = _sampleFiles = new SampleFileCollection(sampleFiles);
	}
	private readonly SampleFileCollection _sampleFiles;

	public void AddSampleFile(SampleFile sampleFile) => _sampleFiles.Add(sampleFile);
}