# LevelEditor

Editor genérico de tilemaps

## Requisitos
Lazarus 3.4, pl_opengl, BGRABitmap

## Formato do level
Abaixo segue um exemplo de como está estruturado um level

``` json
{
   "app":"LevelEditor",
   "version":"0.01",
   "tilesize":"16",
   "width":"20",
   "height":"10",
   "name":"DemoLevel01",
   "layer":[
    {
        "name":"background",
        "width":"20",
        "height":"10",
        "texture":"tileset_1616.png",
        "data":"6,6,6,6,6,6,6,6,6,6,6,-1,-1,-1,-1,-1,-1,-1,-1,6,6,-1,-1,-1,-1,-1,-1,-1,-1,6,6,-1,-1,-1,-1,-1,-1,-1,-1,6,6,6,6,6,6,6,6,6,6,6,6",
        "gameobject":[
            { "name":"player", "x":"10", "y":"10" },
            { "name":"coin", "x":"16", "y":"10" },
            { "name":"enemy1", "x":"32", "y":"10" },
            { "name":"enemy1", "x":"128", "y":"10" }
        ]
    }
   ]
}
```

## Contributing

Pull requests are welcome. For major changes, please open an issue first
to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License

[MIT](https://choosealicense.com/licenses/mit/)