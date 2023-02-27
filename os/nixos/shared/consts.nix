rec {
  email = "";
  name  = "";

  user     = "neeasade";
  home     = "/home/${user}";
  dotfiles = "${home}/.dotfiles";

  sshKeys  = [
               "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCojiWaypftE0EGubAGZbXlEh6L1ehNWiD2NiwhTk6sVzHrXJEy1GB3kFep5wCnm0iV+ks8xdKZQBCdeCswfGGWMfnHicdarHEGYuF8uhU7MW0m9IfK7W80lJfhclyf0cBI+N3CL7zw6cDYmjThKARUP0X8iusViyx2hWQF+6vnV92+ak0xbOC5OTJWx27PC6LzTKIrJZdYX7AF4V/NvyiDfw8+BPNCijhcj8hiKFjYyHWGAwD6R6/qxVR39F8Clqg1ygyIgB5g+RaRN0LyLorSEuucS1oigEhkSZ4m+j1CMsHanXuhgxGjbDpdhVw8BNcL7PYvImB4xtmV41qlWR9r neeasade@littleapple"
             ];

  gpgKeys  = {
    signing = "";
  };

  syncthingDevices = {
    geloof = {
      id = "5TSVINF-TJQWJOR-4UBDHAH-J3EQ2YU-OY5C5AF-3P25BH5-UKXTMJE-PLSP6QO";
    };

    erasmus = {
      id = "4MS4KGQ-X6RBE6E-XX6AOPH-VJXFZ7P-VJ7JC53-35PHBDS-NJK4JRO-LPWHPAN";
    };

    trouw.id = "NGZND66-ZLQFRIH-M6W77CS-FTHYQWT-EGYGD6S-V6MGTS2-K7JUCWC-W4DTTAL";

    phone = {
      # id = "NQOFNTU-DZ3HCGL-P25F5G2-NBIKBON-AMGH2LV-FZGJR6W-YEAVVDY-ICARSA2";
      id = "ORECEL6-GP4MDUV-SFLXPI2-3B2KRFS-52VSIT4-S6FG4D2-2Q4Z5Z3-YRRQQQF";
      introducer = true;
    };
  };
}
