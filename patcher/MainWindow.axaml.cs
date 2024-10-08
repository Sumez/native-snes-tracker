using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Interactivity;
using Avalonia.Platform.Storage;
using Brewsic;
using Brewsic.Playback;
using Patcher.ViewModels;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Patcher;
public partial class MainWindow : Window
{
	private static string[] ValidSampleFiles = new[] { ".brr", ".wav", ".mp3", ".spc", ".sfc", ".smc", ".brrp" };
	public static int PatchedSamplesStartAddress = 0x081E00; // Get this dynamically by reading from another address in file
	public Patch Patch { get; set; }
	public PreviewPlayer PreviewPlayer { get; set; }

	public MainWindow()
	{
		InitializeComponent();
		DataContext = Patch = new Patch();
		PreviewPlayer = new PreviewPlayer();
		PreviewPlayer.PlaySilence();

//		AddBrrfile(File.OpenRead("../../../../samples/perc_snare.brr"), "perc_snare");
//		Patch.RomFilePath = "23";
//		AddUncompressedAudio("../../../../samples/perc_snare.wav", "perc_snare");

		DragDrop.AllowDropProperty.OverrideDefaultValue<MainWindow>(true);

		SampleSources.AddHandler(DragDrop.DropEvent, SampleDrop);
		SampleSources.AddHandler(DragDrop.DragOverEvent, (s, e) =>
		{
			var files = e.Data.GetFiles()?.OfType<IStorageFile>();
			e.DragEffects = files != null && files.Any(f => ValidSampleFiles.Contains(Path.GetExtension(f.Name).ToLower())) ? DragDropEffects.Link : DragDropEffects.None;
			e.Handled = true;
		});
		SampleSources.AddHandler(DragDrop.DragEnterEvent, (s, e) => SampleSources.Classes.Set("DragOver", true));
		SampleSources.AddHandler(DragDrop.DragLeaveEvent, (s, e) => SampleSources.Classes.Set("DragOver", false));
		AddHandler(DragDrop.DragOverEvent, DragOver);
		AddHandler(DragDrop.DropEvent, RomDrop);
	}

	private void DragOver(object? sender, DragEventArgs e)
	{
		var files = e.Data.GetFiles()?.OfType<IStorageFile>();
		if (Patch.RomFilePath != null || files == null || files.Count() != 1 || Path.GetExtension(files.First().Name).ToLower() != ".sfc") e.DragEffects = DragDropEffects.None;
	}
	private async void RomDrop(object? sender, DragEventArgs e)
	{
		if (Patch.RomFilePath != null) return;
		var files = e.Data.GetFiles()?.OfType<IStorageFile>();
		if (files == null || files.Count() != 1 || Path.GetExtension(files.First().Name).ToLower() != ".sfc") return;
		await UseRomFile(files.First());
	}
	private void SampleDrop(object? sender, DragEventArgs e)
	{
		SampleSources.Classes.Set("DragOver", false);
		var files = e.Data.GetFiles()?.OfType<IStorageFile>();
		if (files != null && files.Any(f => ValidSampleFiles.Contains(Path.GetExtension(f.Name).ToLower()))) AddSampleFiles(files);
	}
	public async void PickRomImage(object? sender, RoutedEventArgs args)
	{
		var files = await StorageProvider.OpenFilePickerAsync(new FilePickerOpenOptions
		{
			Title = "Find tracker ROM",
			AllowMultiple = false,
			FileTypeFilter = new[] { new FilePickerFileType("Super Chrono Tracker BSDJ '94") { Patterns = new[] { "bsdj.sfc" } } }
		});

		if (files.Count != 1) return;
		await UseRomFile(files[0]);
	}

	public async void ExportSamplePack(object? sender, RoutedEventArgs args)
	{
		using var sourceStream = await GetSamplePackStream();
		if (sourceStream == null) return;
		var file = await StorageProvider.SaveFilePickerAsync(new FilePickerSaveOptions
		{
			Title = "Export BRR samples with headers to single embeddable file",
			SuggestedFileName = "samples.brrp",
			ShowOverwritePrompt = true,
			DefaultExtension = "brrp"
		});
		if (file == null) return;

		using var stream = await file.OpenWriteAsync();
		sourceStream.CopyTo(stream);
		await new MessageBox($"Sample pack exported to {file.Name}!").ShowDialog(this);
	}
	public async void PatchRom(object? sender, RoutedEventArgs args)
	{
		if (Patch.RomFilePath == null) return;
		using var sourceStream = await GetSamplePackStream();
		if (sourceStream == null) return;

		using var stream = File.OpenWrite(Patch.RomFilePath);
		if (stream.Length < PatchedSamplesStartAddress + sourceStream.Length)
		{
			await new MessageBox("Error: ROM file is too small to contain all selected samples.").ShowDialog(this);
			return;
		}
		stream.Position = PatchedSamplesStartAddress;
		sourceStream.CopyTo(stream);

		await new MessageBox("ROM patched successfully!").ShowDialog(this);
	}

	private async Task<Stream?> GetSamplePackStream()
	{
		var selectedSamples = Patch.AvailableSamples.Where(s => s.Selected && s.BrrSample != null);

		var duplicateSampleNames = selectedSamples.GroupBy(s => s.Name.Substring(0, Math.Min(s.Name.Length, 14))).Where(g => g.Count() > 1);
		if (duplicateSampleNames.Any())
		{
			var message = new StringBuilder("Name conflict issue: Several samples start with this as the first 14 character:\n");
			foreach (var group in duplicateSampleNames) message.Append(group.Key).Append('\n');
			message.Append("\nThe tracker uses the first part of the name to tell which samples are used\nby a song. Please rename or remove duplicates.");
			await new MessageBox(message.ToString()).ShowDialog(this);
			return null;
		}

		var writer = new BinaryWriter(new MemoryStream());
		foreach (var sample in selectedSamples)
		{
			if (sample.BrrSample == null) continue;

			writer.Write(CharMap.FromString(sample.Name));
			writer.Write((byte)0xFF);
			writer.Write((Int16)sample.BrrSample.SampleData.Length);
			writer.Write((Int16)sample.PitchAdjust);
			writer.Write((Int16)sample.BrrSample.LoopStart);
			writer.Write(sample.BrrSample!.SampleData);
		}
		// Write empty sample at the end to indicate end of data
		writer.Write((byte)0xFF);
		writer.Write((Int16)0);
		writer.BaseStream.Position = 0;
		return writer.BaseStream;
	}

	private async Task UseRomFile(IStorageFile file)
	{
		Patch.RomFilePath = file.Path.LocalPath;
		await using var stream = await file.OpenReadAsync();
		stream.Position = PatchedSamplesStartAddress;
		SetSamplesFromStream(stream);
	}

	private void SetSamplesFromStream(Stream stream)
	{
		Patch.AvailableSamples.Clear();
		SampleFile? extractedSample;
		do
		{
			extractedSample = GetSampleFromTracker(stream);
			if (extractedSample != null) Patch.AvailableSamples.Add(extractedSample);
		} while (extractedSample != null);
	}

	private SampleFile? GetSampleFromTracker(Stream stream)
	{
		var name = new StringBuilder();
		var nextByte = stream.ReadByte();
		while (nextByte != 0xFF && stream.Position < stream.Length) { 
			name.Append(CharMap.FromByte(nextByte));
			nextByte = stream.ReadByte();
		}

		var size = stream.ReadByte() | (stream.ReadByte() << 8);
		if (size == 0) return null;
		var pitchAdjust = stream.ReadByte() | (stream.ReadByte() << 8);
		var loopOffset = stream.ReadByte() | (stream.ReadByte() << 8);
		var sampleData = new byte[size];
		stream.Read(sampleData, 0, size);
		var brrSample = new Brewsic.BrrSample(sampleData, loopOffset);
		return new SampleFile(name.ToString(), brrSample) { Expanded = false, PitchAdjust = pitchAdjust };
	}

	public async void PickSource(object? sender, RoutedEventArgs args)
	{
		var files = await StorageProvider.OpenFilePickerAsync(new FilePickerOpenOptions
		{
			Title = "Select samples to add",
			AllowMultiple = true,
			FileTypeFilter = new[] { new FilePickerFileType("Any supported file") { Patterns = ValidSampleFiles.Select(e => $"*{e}").ToList() } }
		});
		await AddSampleFiles(files);
	}

	private async Task AddSampleFiles(IEnumerable<IStorageFile> files)
	{
		foreach (var file in files) await AddSampleFile(file);
	}

	private async Task AddSampleFile(IStorageFile file)
	{
		// TODO: Try-catch
		var name = Path.GetFileNameWithoutExtension(file.Name);
		await using var stream = await file.OpenReadAsync();
		// Reads all the content of file as a text.
		switch (Path.GetExtension(file.Name).ToLower())
		{
			case ".sfc":
				await ParseSnesRom(stream, name);
				break;
			case ".spc":
				await ParseSpcDump(stream, name);
				break;
			case ".brr":
				await AddBrrfile(stream, name);
				break;
			case ".wav":
			case ".mp3":
				AddUncompressedAudio(file.Path.LocalPath, name);
				break;
			case ".brrp":
				AddSamplePack(stream);
				break;
		}
	}
	private void AddSamplePack(Stream stream) => SetSamplesFromStream(stream);

	private class SampleReference
	{
		public int StartAddress { get; set; }
		public int EndAddress { get; set; }
		public bool SuspectedInvalid { get; set; }
		public bool Loops { get; set; }
		public Brewsic.BrrSample Sample { get; set; }
	}
	private async Task ParseSpcDump(Stream stream, string name)
	{
		stream.Position = 0x1015D;
		var directoryAddress = stream.ReadByte() << 8;

		stream.Position = 0x100;
		var buffer = new byte[0x10000];
		await stream.ReadAsync(buffer, 0, 0x10000);

		var dialog = new ImportSamples();
		var dialogTask = dialog.ShowDialog(this);

		var parsedSamples = new List<SampleReference>();

		while (directoryAddress < 0xFFFD)
		{
			var sampleAddress = buffer[directoryAddress] | (buffer[directoryAddress + 1] << 8);
			var loopAddress = buffer[directoryAddress + 2] | (buffer[directoryAddress + 3] << 8);
			var loopStart = loopAddress - sampleAddress;
			directoryAddress += 4;

			stream.Position = 0x100 + sampleAddress + 1;
			var sampleSize = await DetectSampleData(stream);
			if (sampleSize == 0) continue;

			var suspectInvalid = false;
			var endAddress = sampleAddress + sampleSize;

			var brrSampleData = new byte[sampleSize];
			stream.Position = 0x100 + sampleAddress;
			await stream.ReadAsync(brrSampleData, 0, sampleSize);

			var loops = (brrSampleData[sampleSize - 9] & 0x03) == 3;
			if (loopStart < 0 || loopStart > sampleSize || (loopStart % 9) != 0)
			{
				// Invalid loop start. If sample is set to loop, assume it's invalid
				loopStart = 0;
				if (loops) suspectInvalid = true;
			}
			if (brrSampleData.Length < 600 && !loops) suspectInvalid = true; // Sample seems too short to not loop

			var conflictingSample = parsedSamples.FirstOrDefault(e => e.StartAddress < endAddress && e.EndAddress > sampleAddress);
			if (conflictingSample != null)
			{
				if (!conflictingSample.SuspectedInvalid || suspectInvalid) continue;
				parsedSamples.Remove(conflictingSample); // Remove conflicting sample if suspected invalid and this one isn't. Otherwise continue
			}
			var sample = new BrrSample(brrSampleData, loopStart);
			parsedSamples.Add(new SampleReference { 
				StartAddress = sampleAddress,
				EndAddress = endAddress,
				SuspectedInvalid = suspectInvalid,
				Loops = loops,
				Sample = sample
			});
		}
		if (parsedSamples.Count == 0)
		{
			await new MessageBox("Didn't detect any BRR samples in source file - sorry :(").ShowDialog(dialog);
			dialog.Close();
		}
		for (var i = 0; i < parsedSamples.Count; i++)
		{
			var sampleRef = parsedSamples[i];
			var sampleName = $"{name} {i.ToString().PadLeft(2, '0')}";
			//sampleName = parsedSamples[i].StartAddress.ToString("X4") + "->" + parsedSamples[i].EndAddress.ToString("X4");
			if (sampleRef.SuspectedInvalid && sampleRef.Loops) sampleName += " (invalid loop point)";
			var sampleFile = new SampleFile(sampleName, sampleRef.Sample) { Selected = !sampleRef.SuspectedInvalid };
			dialog.AddSampleFile(sampleFile);
		}
		await dialogTask;
		
		foreach (var sample in dialog.SamplesToAddToPatch) Patch.AvailableSamples.Add(sample);
	}

	private async Task AddBrrfile(Stream stream, string name)
	{
		var loopStart = 0;
		var size = stream.Length;
		if (size % 9 == 2)
		{
			size -= 2;
			loopStart = stream.ReadByte() | (stream.ReadByte() << 8);
		}
		var brrSampleData = new byte[size];
		await stream.ReadAsync(brrSampleData, 0, brrSampleData.Length);
		var sample = new Brewsic.BrrSample(brrSampleData, loopStart);
		Patch.AvailableSamples.Add(new SampleFile(name, sample));
	}

	private void AddUncompressedAudio(string path, string name)
	{
		Patch.AvailableSamples.Add(new SampleFile(name, path));
	}

	private async Task ParseSnesRom(Stream stream, string name)
	{
		var dialog = new ImportSamples();
		var dialogTask = dialog.ShowDialog(this);

		var headerBuffer = new byte[1];
		var counter = 0;
		while (stream.Position < stream.Length)
		{
			await stream.ReadAsync(headerBuffer, 0, 1);
			if (headerBuffer[0] == 0xC0) // Could be a BRR header
			{
				var sampleSize = await DetectSampleData(stream, true, true);
				if (sampleSize > 300)
				{
					var brrSampleData = new byte[sampleSize];
					stream.Seek(-1, SeekOrigin.Current);
					await stream.ReadAsync(brrSampleData, 0, sampleSize);
					var sample = new Brewsic.BrrSample(brrSampleData);

					dialog.AddSampleFile(new SampleFile($"{name} {counter++.ToString().PadLeft(2, '0')}", sample) { Selected = false });
				}
			}
		}
		if (counter == 0)
		{
			await new MessageBox("Didn't detect any BRR samples in source file - sorry :(").ShowDialog(dialog);
			dialog.Close();
		}
		await dialogTask;
		foreach (var sample in dialog.SamplesToAddToPatch) Patch.AvailableSamples.Add(sample);
	}

	private async Task<int> DetectSampleData(Stream stream, bool checkForSilentFirstSample = false, bool checkForLikelyBlockHeaders = false)
	{
		var buffer = new byte[1];
		var position = stream.Position;
		await stream.ReadAsync(buffer, 0, 1);
		if (checkForSilentFirstSample && buffer[0] != 0x00)
		{
			stream.Position = position;
			return 0;
		}
		var firstHeaderPosition = position - 1;
		for (var i = 0; i < (0x10000 / 9); i++)
		{
			stream.Position = firstHeaderPosition + i * 9;
			await stream.ReadAsync(buffer, 0, 1);
			if (checkForLikelyBlockHeaders && i <= 2 && (buffer[0] & 0xF1) != 0xC0)
			{
				stream.Position = position;
				return 0;
			}
			if ((buffer[0] & 0x01) == 0x01)
			{
				stream.Position = position;
				var sampleSize = i * 9 + 9;
				return sampleSize > 300 && sampleSize < 0x10000 ? sampleSize : 0;
			}
		}

		stream.Position = position;
		return 0;
	}
}