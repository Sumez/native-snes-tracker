using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;

namespace Patcher;

public partial class MessageBox : Window
{
    public MessageBox(string message)
    {
        InitializeComponent();
		Message = message;
	}

	public string Message { get; }
}