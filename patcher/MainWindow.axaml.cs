using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Interactivity;
using Avalonia.Platform.Storage;
using Brewsic.Playback;
using Patcher.ViewModels;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Patcher;
public partial class MainWindow : Window
{
	public static int PatchedSamplesStartAddress = 0x081900; // Get this dynamically by reading from another address in file
	public Patch Patch { get; set; }
	public PreviewPlayer PreviewPlayer { get; set; }

	public MainWindow()
	{
		InitializeComponent();
		DataContext = Patch = new Patch();
		PreviewPlayer = new PreviewPlayer();
		PreviewPlayer.PlaySilence();

		AddBrrfile(File.OpenRead("../../../../samples/perc_snare.brr"), "perc_snare");
//		Patch.RomFilePath = "23";
		AddUncompressedAudio("../../../../samples/perc_snare.wav", "perc_snare");

		DragDrop.AllowDropProperty.OverrideDefaultValue<MainWindow>(true);

		SampleSources.AddHandler(DragDrop.DropEvent, SampleDrop);
		AddHandler(DragDrop.DragOverEvent, DragOver);
		SampleSources.AddHandler(DragDrop.DragEnterEvent, (s, e) => SampleSources.Classes.Set("DragOver", true));
		SampleSources.AddHandler(DragDrop.DragLeaveEvent, (s, e) => SampleSources.Classes.Set("DragOver", false));
		AddHandler(DragDrop.DropEvent, RomDrop);
	}

	private void DragEnter(object? sender, DragEventArgs e) { e.DragEffects = DragDropEffects.Link; }
	private void DragLeave(object? sender, DragEventArgs e) => SampleSources.Classes.Set("dragover", false);
	private void DragOver(object? sender, DragEventArgs e)
	{
		var files = e.Data.GetFiles()?.OfType<IStorageFile>();
		if (Patch.RomFilePath == null)
		{
			if (files == null || files.Count() != 1 || Path.GetExtension(files.First().Name).ToLower() != ".sfc")
			{
				e.DragEffects = DragDropEffects.None;
			}
			return;
		}

		if (e.Source != SampleSources) return;
		SampleSources.Classes.Set("dragover", true);
		if (files == null || !files.Any(f => ValidSampleFiles.Contains(Path.GetExtension(f.Name).ToLower())))
		{
			e.DragEffects = DragDropEffects.None;
		}
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

		if (files.Count < 1) return;
		await UseRomFile(files[0]);
	}
	public async void PatchRom(object? sender, RoutedEventArgs args)
	{
		if (Patch.RomFilePath == null) return;

		var selectedSamples = Patch.AvailableSamples.Where(s => s.Selected && s.BrrSample != null);

		var duplicateSampleNames = selectedSamples.GroupBy(s => s.Name).Where(g => g.Count() > 1);
		if (duplicateSampleNames.Any())
		{
			var message = new StringBuilder("Error: The following sample names are duplicated:\n");
			foreach (var group in duplicateSampleNames) message.Append(group.Key).Append('\n');
			message.Append("\nPlease rename or remove duplicates.");
			await new MessageBox(message.ToString()).ShowDialog(this);
			return;
		}

		using var writer = new BinaryWriter(new MemoryStream());
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

		using var stream = File.OpenWrite(Patch.RomFilePath);
		if (stream.Length < PatchedSamplesStartAddress + writer.BaseStream.Length)
		{
			await new MessageBox("Error: ROM file is too small to contain all selected samples.").ShowDialog(this);
			return;
		}
		stream.Position = PatchedSamplesStartAddress;
		writer.BaseStream.Position = 0;
		writer.BaseStream.CopyTo(stream);

		await new MessageBox("ROM patched successfully!").ShowDialog(this);
	}
	private async Task UseRomFile(IStorageFile file)
	{
		Patch.RomFilePath = file.Path.LocalPath;
		await using var stream = await file.OpenReadAsync();
		stream.Position = PatchedSamplesStartAddress;
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
		while (nextByte != 0xFF) { 
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

	private static string[] ValidSampleFiles = new[] { ".brr", ".wav", ".mp3", ".spc", ".sfc", ".smc" };

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
		}
	}

	private async Task ParseSpcDump(Stream stream, string name)
	{
		var counter = 0;
		stream.Position = 0x1015D;
		var directoryAddress = stream.ReadByte() << 8;

		stream.Position = 0x100;
		var buffer = new byte[0x10000];
		await stream.ReadAsync(buffer, 0, 0x10000);


		var dialog = new ImportSamples();
		var dialogTask = dialog.ShowDialog(this);

		while (directoryAddress < 0xFFFD)
		{
			var sampleAddress = buffer[directoryAddress] | (buffer[directoryAddress + 1] << 8);
			var loopAddress = buffer[directoryAddress + 2] | (buffer[directoryAddress + 3] << 8);
			var loopStart = loopAddress - sampleAddress;

			stream.Position = 0x100 + sampleAddress + 1;
			var sampleSize = await DetectSampleData(stream);
			if (sampleSize > 0)
			{

				var brrSampleData = new byte[sampleSize];
				stream.Position = 0x100 + sampleAddress;
				await stream.ReadAsync(brrSampleData, 0, sampleSize);

				if (loopStart < 0 || loopStart > sampleSize || (loopStart % 9) != 0) loopStart = 0;
				var sample = new Brewsic.BrrSample(brrSampleData, loopStart);
				dialog.AddSampleFile(new SampleFile($"{name} {counter++.ToString().PadLeft(2, '0')}", sample) { Selected = false });
			}
			directoryAddress += 4;
		}
		if (counter == 0)
		{
			await new MessageBox("Didn't detect any BRR samples in source file - sorry :(").ShowDialog(dialog);
			dialog.Close();
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