using Avalonia;
using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Markup.Xaml;
using System;

namespace Patcher;

public partial class MessageBox : Window
{
    public MessageBox(string message)
    {
        InitializeComponent();
		Message.Text = message;
	}

	private void CloseWindow(object? sender, RoutedEventArgs e) => Close();
}