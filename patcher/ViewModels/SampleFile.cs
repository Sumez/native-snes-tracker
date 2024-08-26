using Brewsic;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Threading;

namespace Patcher.ViewModels;

public class SampleFileCollection : ObservableCollection<SampleFile>, INotifyPropertyChanged
{
	public SampleFileCollection() : base() { }
	public SampleFileCollection(IEnumerable<SampleFile> sampleFiles) : base(sampleFiles) { }
}

public class SampleFile : INotifyPropertyChanged
{
	private string _name;
	public string Name { get => _name; set { _name = value; if (PropertyChanged != null) PropertyChanged(this, new PropertyChangedEventArgs(nameof(Name))); } }
	public int CompressionBitRate { get; set; } = 144;
	public string? UncompressedSourceFile { get; private set; }
	private bool _expanded = true;
	public bool Expanded
	{
		get => _expanded;
		set
		{
			_expanded = value;
			if (PropertyChanged != null)
			{
				PropertyChanged(this, new PropertyChangedEventArgs(nameof(Expanded)));
				PropertyChanged(this, new PropertyChangedEventArgs(nameof(Collapsed)));
			}
		}
	}
	public bool Collapsed => !_expanded;
	public bool Selected { get; set; } = true;
	public BrrSample? BrrSample { get; private set; }
	public int AssignedNote { get; set; } = 4 * 12; // C-4
	public double CompressedSampleRate { get; private set; } = 32000;

	public string CompressedSize =>
		BrrSample == null ? "compressing..."
		: BrrSample.SampleData.Length >= 1000 ? ((double)BrrSample.SampleData.Length / 1024).ToString("0.00 KB")
		: BrrSample.SampleData.Length.ToString("0 bytes");

	public AudioFile? AudioFile { get; private set; }
	public int PitchAdjust
	{
		get
		{
			// Calculate where we'd expect the note to be based on the sample rate, and then adjust the pitch to match the assigned note
			var defaultNote = 12 * Math.Log(CompressedSampleRate / 250) / Math.Log(2); // 250 is C-0, and every time the sample rate doubles, the pitch goes up by 12 (one octave)
			return (int)Math.Round((defaultNote - AssignedNote) * 64); // Pitch is defined by 1/64ths of a semitone. Should give us a fairly decent resolution for handling odd sample rates
		}
		set
		{
			// Reverse the pitch calculation to get compressed sample rate
			var defaultNote = AssignedNote + value / 64.0;
			CompressedSampleRate = 250 * Math.Pow(2, defaultNote / 12);
        }
	}

	public event PropertyChangedEventHandler? PropertyChanged;

	
	protected SampleFile(string name)
	{
		Name = name.Replace('_', ' ');
	}
	public SampleFile(string name, BrrSample brrSample) : this(name)
	{
		BrrSample = brrSample;
	}

	public SampleFile(string name, string uncompressedSourceFile) : this(name)
	{
		UncompressedSourceFile = uncompressedSourceFile;
	}

	public void Resample(int sampleRate, CancellationToken? token = null)
	{
		BrrSample = null;
		CompressedSampleRate = sampleRate;
		if (PropertyChanged != null)
		{
			PropertyChanged(this, new PropertyChangedEventArgs(nameof(CompressedSize)));
			PropertyChanged(this, new PropertyChangedEventArgs(nameof(BrrSample)));
		}
		if (UncompressedSourceFile == null) return;

		AudioFile = AudioFile.LoadFromFile(UncompressedSourceFile, sampleRate, null, token);
		if (token?.IsCancellationRequested ?? true) return;
		GetBrrSampleFromAudioFile(token);
	}

	private void GetBrrSampleFromAudioFile(CancellationToken? token)
	{
		var sampleData = AudioFile.Sample.GetBrr(500, 0, (s) => { }, token);
		if (sampleData == null || (token?.IsCancellationRequested ?? true)) return;
		BrrSample = new BrrSample(sampleData);
		if (PropertyChanged != null)
		{
			PropertyChanged(this, new PropertyChangedEventArgs(nameof(CompressedSize)));
			PropertyChanged(this, new PropertyChangedEventArgs(nameof(BrrSample)));
		}
	}
}
