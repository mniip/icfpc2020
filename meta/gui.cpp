#include <iostream>
#include <vector>
#include <SDL2/SDL.h>

#define XRES 200
#define YRES 200
#define SCALE 5

int main(int argc, char ** argv)
{
	bool leftMouseButtonDown = false;
	bool quit = false;
	bool click = false;
	SDL_Event event;

	SDL_Init(SDL_INIT_VIDEO);

	SDL_Window * window = SDL_CreateWindow("SDL2 Pixel Drawing",
			SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, XRES*SCALE, YRES*SCALE, 0);

	SDL_Renderer * renderer = SDL_CreateRenderer(window, -1, 0);
	SDL_Texture * texture = SDL_CreateTexture(renderer,
			SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STATIC, XRES, YRES);
	//Uint32 * pixels = new Uint32[XRES * YRES];
	std::vector<Uint32*> pixelsList;
	std::vector<std::pair<int,int>> coordList;

	SDL_RenderSetScale(renderer, SCALE, SCALE);

	while(!quit)
	{
		int ux, uy, lx, ly;
		char c;
		int current_pic = 0;

		std::cin >> c;
		switch(c)
		{
			case 'P':
			{

				Uint32 * pixels = new Uint32[XRES * YRES];
				memset(pixels, 0, XRES * YRES * sizeof(Uint32));

				std::cin >> ux >> uy >> lx >> ly;
				for(int x = 0; x <= lx-ux; ++x)
				{
					for(int y = 0; y <= ly-uy; ++y)
					{
						char bit;
						std::cin >> bit;
						pixels[y * XRES + x] = (bit=='1' ? 0xffffff : 0);
					}
				}
				pixelsList.push_back(pixels);
				coordList.push_back(std::make_pair(ux, uy));

				break;
			}
			case 'I':
			{
				click = false;
				while (!quit && !click)
				{
					SDL_UpdateTexture(texture, NULL, pixelsList[current_pic], XRES * sizeof(Uint32));

					SDL_WaitEvent(&event);

					switch (event.type)
					{
						case SDL_QUIT:
							quit = true;
							break;
						case SDL_MOUSEBUTTONDOWN:
						{
							int mouseX = event.motion.x/SCALE;
							int mouseY = event.motion.y/SCALE;

							std::cout << mouseX+coordList[current_pic].first << " "
							          << mouseY+coordList[current_pic].second << std::endl;
							click = true; //One mouse click only
							break;
						}
						case SDL_KEYDOWN:
							switch(event.key.keysym.sym)
							{
								case SDLK_LEFT:
									if(current_pic)
										current_pic--;
									else
										current_pic = pixelsList.size()-1;
									break;
								case SDLK_RIGHT:
									current_pic = (current_pic + 1) % pixelsList.size();
									break;
							}
							break;
					}

					SDL_RenderClear(renderer);
					SDL_RenderCopy(renderer, texture, NULL, NULL);
					SDL_RenderPresent(renderer);
				}

				//Free memory
				for(int i = 0; i < pixelsList.size(); ++i)
					delete[] pixelsList[i];
				pixelsList.clear();
				coordList.clear();

				break;
			}
		}
	}

	SDL_DestroyTexture(texture);
	SDL_DestroyRenderer(renderer);

	SDL_DestroyWindow(window);
	SDL_Quit();

	return 0;
}
