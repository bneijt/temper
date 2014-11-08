![Travis CI build status](https://api.travis-ci.org/bneijt/temper.svg)

Code to monitor a [TEMPer USB thermometer](http://www.dx.com/p/81105?Utm_rid=85466298&Utm_source=affiliate) in Haskell.

    Bus 001 Device 031: ID 0c45:7401 Microdia TEMPer Temperature Sensor
    Device Descriptor:
      bLength                18
      bDescriptorType         1
      bcdUSB               2.00
      bDeviceClass            0 (Defined at Interface level)
      bDeviceSubClass         0
      bDeviceProtocol         0
      bMaxPacketSize0         8
      idVendor           0x0c45 Microdia
      idProduct          0x7401 TEMPer Temperature Sensor
      bcdDevice            0.01
      iManufacturer           1 RDing
      iProduct                2 TEMPerV1.4
      iSerial                 0
      bNumConfigurations      1
      Configuration Descriptor:
        bLength                 9
        bDescriptorType         2
        wTotalLength           59
        bNumInterfaces          2
        bConfigurationValue     1
        iConfiguration          0
        bmAttributes         0xa0
          (Bus Powered)
          Remote Wakeup
        MaxPower              100mA
        Interface Descriptor:
          bLength                 9
          bDescriptorType         4
          bInterfaceNumber        0
          bAlternateSetting       0
          bNumEndpoints           1
          bInterfaceClass         3 Human Interface Device
          bInterfaceSubClass      1 Boot Interface Subclass
          bInterfaceProtocol      1 Keyboard
          iInterface              0
            HID Device Descriptor:
              bLength                 9
              bDescriptorType        33
              bcdHID               1.10
              bCountryCode            0 Not supported
              bNumDescriptors         1
              bDescriptorType        34 Report
              wDescriptorLength      65
             Report Descriptors:
               ** UNAVAILABLE **
          Endpoint Descriptor:
            bLength                 7
            bDescriptorType         5
            bEndpointAddress     0x81  EP 1 IN
            bmAttributes            3
              Transfer Type            Interrupt
              Synch Type               None
              Usage Type               Data
            wMaxPacketSize     0x0008  1x 8 bytes
            bInterval              10
        Interface Descriptor:
          bLength                 9
          bDescriptorType         4
          bInterfaceNumber        1
          bAlternateSetting       0
          bNumEndpoints           1
          bInterfaceClass         3 Human Interface Device
          bInterfaceSubClass      1 Boot Interface Subclass
          bInterfaceProtocol      2 Mouse
          iInterface              0
            HID Device Descriptor:
              bLength                 9
              bDescriptorType        33
              bcdHID               1.10
              bCountryCode            0 Not supported
              bNumDescriptors         1
              bDescriptorType        34 Report
              wDescriptorLength      41
             Report Descriptors:
               ** UNAVAILABLE **
          Endpoint Descriptor:
            bLength                 7
            bDescriptorType         5
            bEndpointAddress     0x82  EP 2 IN
            bmAttributes            3
              Transfer Type            Interrupt
              Synch Type               None
              Usage Type               Data
            wMaxPacketSize     0x0008  1x 8 bytes
            bInterval              10
    Device Status:     0x0000
      (Bus Powered)


On Linux or Raspberry PI you need to have special privileges to access USB devices. To use the device without root privileges, create a file called `/etc/udev/rules.d/50-temper.rules` containing:

    ATTRS{idVendor}=="0c45", ATTRS{idProduct}=="7401", SUBSYSTEMS=="usb", ACTION=="add", MODE="0666", GROUP="plugdev"

After creation execute `sudo udevadm control --reload-rules` to reload the rules and then reconnect the Temper device.
If your user is a member of the `plugdev` group, you should be allowed to access the temper without root privileges.

See also
--------
This code is based on other projects on the internet:
- C code at https://github.com/edorfaus/TEMPered
- Python code at https://github.com/padelt/temper-python
